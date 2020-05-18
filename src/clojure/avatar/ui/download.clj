; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.download
  (:require [clojure.java.io :as io]
            [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [clj-jfx.properties :as props]
            [avatar.algorithms.progress-tracker :as progress]
            [clojure.string :as str]
            [avatar.ui.tools :as t])
  (:import (java.net HttpURLConnection URL SocketTimeoutException)
           (java.io BufferedInputStream File FileNotFoundException RandomAccessFile)
           (javafx.stage WindowEvent)
           (java.nio.file Path Paths Files CopyOption StandardCopyOption LinkOption)
           (java.util TimerTask Timer)))


(defn as-path
  ^Path [x]
  (cond
    (instance? Path x) x
    (instance? File x) (.toPath ^File x)
    (instance? String x) (Paths/get ^String x, (make-array String 0))))


(defn download-file-part
  [target-file, source-url, callback-fn, ^long start-position]
  (let [source-url ^URL (io/as-url source-url)
        buffer-size 8192
        buffer (byte-array buffer-size)
        connection (doto ^HttpURLConnection (.openConnection source-url)
                     (.setRequestProperty "Range", (str "bytes=" start-position "-"))
                     (.setReadTimeout 1000)
                     (.connect))
        target (doto (RandomAccessFile. (io/file target-file), "rw")
                 (.seek start-position))]
    (if (== (quot (.getResponseCode connection) 100) 2)
      (let [size (.getContentLengthLong connection)
            in (BufferedInputStream. (.getInputStream connection))
            thread (Thread/currentThread)
            error (volatile! nil)]
        (callback-fn size, start-position)
        (try
          (loop [bytes-read 0, position start-position]
            (let [n (unchecked-long
                      (try
                        (.read in buffer)
                        (catch InterruptedException e
                          (vreset! error :interrupted)
                          -1)
                        (catch SocketTimeoutException e
                          (vreset! error :timeout)
                          -1)
                        (catch Throwable t
                          (vreset! error :unknown-read-error)
                          -1)))]
              (if (pos? n)
                (let [new-bytes-read (unchecked-add bytes-read, n)
                      new-position (unchecked-add position, n)
                      write-completed (try
                                        (.write target buffer, 0, n)
                                        true
                                        (catch InterruptedException e
                                          (vreset! error :interrupted)
                                          false)
                                        (catch Throwable t
                                          (vreset! error :write-failed)
                                          false))]
                  (if (and write-completed (not (.isInterrupted thread)))
                    (do
                      (callback-fn size, new-position)
                      (recur new-bytes-read, new-position))
                    {:result (or (deref error) (when (.isInterrupted thread) :interrupted))
                     :read-size (if write-completed new-bytes-read bytes-read)
                     :position position}))
                (if-let [error-reason (deref error)]
                  {:result error-reason
                   :read-size bytes-read
                   :position position}
                  {:result (if (== bytes-read size) :completed :incomplete)
                   :read-size bytes-read
                   :position position}))))
          (finally
            (.close in)
            (.disconnect connection)
            (.close target))))
      {:result :request-error
       :read-size 0
       :position start-position})))


(defn download
  [target-file, source-url, callback-fn]
  (let [thread (Thread/currentThread)]
    (loop [current-position 0]
      (let [{:keys [result, position]} (download-file-part target-file, source-url, callback-fn, current-position)]
        (if (and (= result :timeout) (not (.isInterrupted thread)))
          (recur (unchecked-long position))
          (= result :completed))))))



(defn format-speed
  [^double speed]
  (loop [i 0, speed speed]
    (if (< speed 1000.0)
      (format "%.1f %s/s" speed (nth ["B" "KB" "MB" "GB"] i))
      (recur (unchecked-inc i), (/ speed 1000.0)))))


(defn update-progress
  [state-atom
   size
   position]
  (let [now (System/currentTimeMillis)
        {:keys [last-update]} (deref state-atom)]
    (when (>= (- now last-update) 500)
      (swap! state-atom
        (fn [state]
          (cond-> state
            ; record size just once
            (not (contains? state :size))
            (assoc :size size)

            true
            (assoc
              :last-update now
              :downloaded position)))))))


(defn determine-unit
  [^double size]
  (loop [i 0, divisor 1.0, size size]
    (if (< size 1000.0)
      [divisor, (nth ["B" "KB" "MB" "GB"] i)]
      (recur (unchecked-inc i), (* divisor 1000.0) (/ size 1000.0)))))


(defn update-ui
  [{:keys [progress-bar
           progress-label
           size-label
           speed-label
           duration-label]},
   state-atom]
  (jfx/run-later
    (let [now (System/currentTimeMillis)
          {:keys [start-time, downloaded, size]} (deref state-atom)]
      (when (and size (pos? size))
        (let [duration (- now start-time)
              progress (/ (double downloaded) size)
              speed (when (pos? duration)
                      (/ (double downloaded) duration))
              est-duration (when (and speed (pos? speed))
                             (/ (- size downloaded) speed))
              [size-divisor, unit] (determine-unit size)]
          (props/set-property progress-bar, :progress, progress)
          (props/set-property progress-label, :text (format "%.2f%%" (* 100 progress)))
          (props/set-property size-label, :text (format "%.2f / %.2f %s" (/ downloaded size-divisor) (/ size size-divisor) unit))
          (props/set-property speed-label, :text (if speed (format-speed (* 1000 speed)) "N/A"))
          (props/set-property duration-label, :text (if est-duration (progress/duration-str est-duration) "N/A")))))))


(defn download-with-progress
  [title, description, target-file, source-url]
  (let [source-url (io/as-url source-url)
        temp-file (str (u/file-absolute-path target-file) "_TEMP_" (System/currentTimeMillis))
        success?-promise (promise)]
    (jfx/run-now
      (let [control (jfx/create-control "avatar/ui/DownloadView.fxml")
            control-node (jfx/control-node control)
            window (doto (jfx/modal-window title, control-node)
                     (jfx/window-style! :utility)
                     (jfx/handle-event! :window-close-request
                       (fn [^WindowEvent e]
                         (when-not (realized? success?-promise)
                           (.consume e))))
                     (t/add-avatar-icon))
            {:keys [caption-label
                    progress-bar
                    cancel-button]
             :as children} (jfx/control-children control)
            state-atom (atom
                         {:last-update 0})
            timer (Timer.)
            timer-task (proxy [TimerTask] []
                         (run []
                           (update-ui children, state-atom)))
            thread (Thread.
                     (reify Runnable
                       (run [this]
                         (try
                           (let [start-time (System/currentTimeMillis)
                                 _ (swap! state-atom assoc :start-time start-time)
                                 result (download temp-file, source-url, (partial update-progress state-atom))]
                             (.cancel timer-task)
                             (jfx/run-later (jfx/hide window))
                             (deliver success?-promise result))
                           (catch Throwable t
                             (u/log-exception (format "Download of \"%s\" failed." (str source-url)) t)
                             (jfx/show-error "Download failed"
                               (format "Could not download %s" (u/file-name (.getFile source-url)))
                               (cond->> (.getMessage t)
                                 (instance? FileNotFoundException t)
                                 (str "File does not exist:\n")))
                             (jfx/run-now (jfx/hide window))
                             (deliver success?-promise false))))))]
        (props/set-property caption-label, :text description)
        (props/set-property progress-bar, :progress, 0)
        (.scheduleAtFixedRate timer, timer-task, 0, 500)
        (.start thread)
        (jfx/handle-event! cancel-button, :action
          (fn [_]
            (.interrupt thread)))
        (jfx/show window)))
    ; handle outcome
    (let [success? (deref success?-promise)]
      (if success?
        ; rename on success
        (Files/move (as-path temp-file), (as-path target-file), (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
        ; delete temp file on failure (if written)
        (when (Files/exists (as-path temp-file), (make-array LinkOption 0))
          (Files/delete (as-path temp-file))))
      ; return success?
      success?)))