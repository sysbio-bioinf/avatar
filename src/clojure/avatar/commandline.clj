; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.commandline
  (:require
    [avatar.util :as u]
    [clojure.stacktrace :as st]
    [avatar.data.management :as dm]
    [com.stuartsierra.component :as c]
    [frost.quick-freeze :as qf]
    [avatar.version :as v]
    [clojure.string :as str]
    [avatar.algorithms.batch :as batch]
    [avatar.algorithms.progress-tracker :as progress])
  (:import
    (java.io Writer)))





(deftype CommandlineProgressTrackerIndividual [state-atom, index]
  progress/ProgressTracker

  (start [this]
    (u/safe
      (let [now (System/currentTimeMillis)]
        (swap! state-atom update-in [index] assoc
          :start-time now
          :update-time now))))

  (update [this]
    (progress/update this, 1))

  (update [this, completed-steps]
    (u/safe-future
      (swap! state-atom update-in [index]
        #(-> %
           (update-in [:completed-tasks] + completed-steps)
           (update-in [:update-time] max (System/currentTimeMillis))))))

  (finished [this]
    (u/safe-future
      (swap! state-atom update-in [index]
        (fn [{:keys [total-tasks] :as state}]
          (assoc state :completed-tasks total-tasks))))))


(deftype CommandlineProgressTracker [state-atom, individual-progress-trackers]
  progress/MultipleProgressTracker
  (individual-progress-tracker [this, index]
    (nth individual-progress-trackers index)))


(defn show-state-update
  [process-state-list]
  (let [remaining-duration (progress/estimate-total-duration process-state-list)
        update-time (transduce (keep :update-time) max 0 process-state-list)
        start-time (transduce (keep :start-time) min Long/MAX_VALUE process-state-list)
        current-duration (progress/duration-str (- update-time start-time))
        process-count (count process-state-list)
        progress (transduce
                   (map
                     (fn [{:keys [completed-tasks, total-tasks]}]
                       (/ (double completed-tasks) total-tasks)))
                   (completing + #(/ % process-count))
                   process-state-list)
        completed-processes (transduce
                              (map
                                (fn [{:keys [completed-tasks, total-tasks]}]
                                  (if (= completed-tasks total-tasks)
                                    1
                                    0)))
                              (completing +)
                              process-state-list)]
    (print
      (format (str "\rRuns: %" (count (str process-count)) "d/%d  -  Progress: %6.3f%%  -  Current Duration: %s  -  Remaining Duration: %-20s")
        completed-processes
        process-count
        (* 100.0 progress)
        current-duration
        remaining-duration))
    (flush)))


(defn commandline-progress-tracker
  [caption, process-info-list]
  (let [state-atom (atom
                     (mapv
                       (fn [{:keys [name, total-tasks, difficulty]}]
                         {:completed-tasks 0,
                          :total-tasks total-tasks,
                          :start-time nil,
                          :update-time nil,
                          :name name,
                          :difficulty difficulty})
                       process-info-list))
        print-agent (agent nil)]

    (add-watch state-atom :estimate-duration
      (fn [_, _, old-state, new-state]
        (when-not (= old-state new-state)
          (send print-agent
            (fn [_]
              (show-state-update new-state)
              nil)))))

    ; print caption
    (println caption)

    (CommandlineProgressTracker.
      state-atom,
      (mapv #(CommandlineProgressTrackerIndividual. state-atom, %) (range (count process-info-list))))))



(defn flush-err
  []
  (.flush ^Writer *err*))


(defn fail
  [code, fmt, & args]
  (u/println-err (apply format fmt args))
  (flush-err)
  (System/exit code))


(defn check-file-version
  [file]
  (let [{:keys [version]} (qf/quick-file-info file),
        prog-version (v/current-version-data)]
    (when-not (= (dissoc version :snapshot?) (dissoc prog-version :snapshot?))
      (fail 21 "File version \"%s\" required but version \"%s\" found for \"%s\"."
        (v/version-str prog-version), (v/version-str version), (u/file-absolute-path file)))))


(defn run-batch-optimization
  [thread-count, batch-config-file, data-file]
  (when-not (u/file-exists? batch-config-file)
    (fail 10 "Batch config file \"%s\" does not exist!" (u/file-absolute-path batch-config-file)))
  (when-not (u/file-exists? data-file)
    (fail 20 "Data file \"%s\" does not exist!" (u/file-absolute-path data-file)))
  (check-file-version data-file)
  (let [batch-config (try
                       (read-string (slurp batch-config-file))
                       (catch Throwable t
                         (fail 11 "Could not load batch config from \"%s\".\n%s"
                           (u/file-absolute-path batch-config-file)
                           (with-out-str (st/print-cause-trace t)))))
        batch-config (assoc-in batch-config [:batch, :thread-count] (or thread-count 1))
        data-mgmt (c/start (dm/create-data-management))]
    (try
      (dm/load-from-file data-mgmt, data-file)
      (catch Throwable t
        (fail 22 "Could not load alteration data from \"%s\".\n%s"
          (u/file-absolute-path batch-config-file)
          (with-out-str (st/print-cause-trace t)))))
    (let [snapshots (dm/snapshots data-mgmt),
          batch (batch/execute-batch snapshots, batch-config, commandline-progress-tracker)
          runs-with-errors (filterv :error (:pareto-front-list batch))]
      (println)
      (when (seq runs-with-errors)
        (println
          (format "Batch execution: %s errors occured during the execution of batch \"%s\":\n  %s"
            (count runs-with-errors) (:name batch)
            (str/join "\n  " (mapv :error runs-with-errors)))))
      (dm/save-batch data-mgmt, (update-in batch [:pareto-front-list] (partial u/removev :error)))
      (try
        (let [save-file (u/amend-filename data-file ".optimized")]
          (println (format "Saving result to \"%s\" ..." (u/file-absolute-path save-file)))
          (dm/save-to-file data-mgmt, save-file)
          (println "done"))
        (catch Throwable t
          (fail 30 "Failed to save file with optimization results."))))

    (System/exit 0)))