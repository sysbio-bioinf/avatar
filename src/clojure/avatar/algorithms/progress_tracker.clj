; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.progress-tracker
  (:refer-clojure :exclude [update])
  (:require
    [avatar.util :as u]))



(defprotocol ProgressTracker
  (start [this])
  (update [this] [this, completed-steps])
  (finished [this]))


(defprotocol MultipleProgressTracker
  (individual-progress-tracker [this, index]))


(defn duration-str
  [duration]
  (let [{:keys [days hours minutes seconds]} (u/duration-data duration)]
    (if (pos? days)
      (format "%02dd %02d:%02d:%02d" days, hours, minutes, seconds)
      (format "%02d:%02d:%02d" hours, minutes, seconds))))


(defn computation-speed
  [{:keys [completed-tasks, start-time, update-time, difficulty]}]
  (if (and start-time update-time (not= start-time update-time))
    (/ (* (double completed-tasks) difficulty)
      (- update-time start-time))
    0.0))


(defn process-activity
  [{:keys [completed-tasks, total-tasks]}]
  (if (zero? completed-tasks)
    :waiting
    (if (< completed-tasks total-tasks)
      :running
      :completed)))


(defn completing-nil-safe
  [f]
  (fn
    ([] nil)
    ([a] a)
    ([a, b]
     (if (and a b)
       (f a b)
       (or a b)))))


(defn estimate-concurrency
  [state-list]
  (let [min-start-time (transduce
                         (keep :start-time)
                         (completing-nil-safe min)
                         state-list)
        max-update-time (transduce
                          (keep :update-time)
                          (completing-nil-safe max)
                          state-list)
        duration-sum (transduce
                       (keep
                         (fn [{:keys [start-time, update-time]}]
                           (when (and start-time update-time)
                             (- update-time start-time))))
                       (completing +)
                       state-list)]
    (if (and min-start-time max-update-time (not= min-start-time max-update-time))
      (/ (double duration-sum) (- max-update-time min-start-time))
      1.0)))


(defn remaining-duration
  [completed-tasks, total-tasks, start-time, update-time]
  (when (and start-time update-time (> completed-tasks 0))
    (let [duration (- update-time start-time)
          speed (/ (double completed-tasks) duration),
          remaining (- total-tasks completed-tasks)]
      (long (/ remaining speed)))))


(defn estimate-total-duration
  [state-list]
  (let [{:keys [waiting, running, completed]} (group-by process-activity state-list)]
    ; done?
    (if (and (empty? waiting) (empty? running))
      ; zero duration
      (duration-str 0)
      ; estimate durations and report max duration
      (let [speed-values (into []
                           (comp
                             (map computation-speed)
                             (remove zero?))
                           (concat running completed))
            estimated-speed (when (seq speed-values)
                              (reduce min speed-values))]
        (if estimated-speed
          (let [duration-values (into
                                  (mapv
                                    (fn [{:keys [completed-tasks, total-tasks, start-time, update-time] :as state}]
                                      (remaining-duration completed-tasks, total-tasks, start-time, update-time))
                                    running)
                                  (mapv
                                    (fn [{:keys [total-tasks, difficulty] :as state}]
                                      (/ (* total-tasks difficulty) estimated-speed))
                                    waiting))
                duration-sum (reduce + duration-values),
                concurrency (estimate-concurrency running)]
            (duration-str (/ duration-sum concurrency)))
          "N/A")))))