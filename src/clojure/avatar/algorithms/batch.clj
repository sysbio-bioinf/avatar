; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.batch
  (:require
    [avatar.data.gene-data :as gd]
    [avatar.algorithms.common :as c]
    [clojure.set :as set]
    [com.climate.claypoole :as cp]
    [avatar.util :as u]
    [avatar.algorithms.progress-tracker :as progress]
    [avatar.algorithms.mo-select :as mo-select]))


; multi-objective-gene-selection


(defn snapshot-runs
  [batch-config
   {:keys [selected-alteration-type
           selected-second-objective
           optimization-on-all-samples?
           minimum-alteration-ratio]
    :as optimization-setup}
   selected-sample-group-names
   {:keys [data], snapshot-name :name, :as snapshot}]
  (let [snapshot-sample-groups (gd/sample-groups data)
        gene-count (-> data :gene-list count)]
    (mapv
      (fn [sample-group]
        {:run-name [snapshot-name
                    (gd/alteration-type-str selected-alteration-type)
                    (name selected-second-objective)
                    (format "%.2f" minimum-alteration-ratio)
                    (or sample-group "ALL")]
         :snapshot-name snapshot-name,
         :gene-sample-data data
         :setup-data (merge
                       {:alteration-type selected-alteration-type
                        :second-objective selected-second-objective
                        :minimum-alteration-ratio minimum-alteration-ratio
                        :select-only-for-sample-group sample-group,
                        :exclude-fixed-rows? true,
                        :thread-count 1}
                       (c/algorithm-parameter-config batch-config, gene-count))})
      (cond-> (set/intersection selected-sample-group-names, snapshot-sample-groups)
        optimization-on-all-samples?
        (conj nil)))))


(defn optimization-setup->runs
  [id->snapshot, batch-config, optimization-setup]
  (let [{:keys [selected-snapshots, selected-sample-groups]} optimization-setup,
        selected-sample-group-names (into #{} (map :sample-group) selected-sample-groups)]
    (mapcat
      (fn [{:keys [id] :as selected-snapshot}]
        (snapshot-runs batch-config, optimization-setup, selected-sample-group-names, (get id->snapshot id)))
      selected-snapshots)))


(defn optimization-run-configs
  [snapshot-list, batch-config, optimization-setup-list]
  (let [id->snapshot (zipmap (mapv :id snapshot-list) snapshot-list)]
    (mapcat
      (partial optimization-setup->runs id->snapshot, batch-config)
      optimization-setup-list)))


(defn run-optimization
  [multiple-progress-tracker, {:keys [run-name, snapshot-name, gene-sample-data, setup-data, index] :as run-config}]
  (let [progress-tracker (when multiple-progress-tracker
                           (doto (progress/individual-progress-tracker multiple-progress-tracker, index)
                             (progress/start)))
        result (try
                 (mo-select/multi-objective-gene-selection gene-sample-data,
                   (cond-> setup-data
                     multiple-progress-tracker
                     (assoc :progress-tracker progress-tracker)))
                 (catch Throwable t
                   (let [msg (format "Exception in batch run \"%s\" for snapshot \"%s\". Parameters:\n%s"
                               run-name, snapshot-name, (pr-str setup-data))]
                     (u/log-exception msg, t)
                     msg))
                 (finally
                   (progress/finished progress-tracker)))]
    (cond-> {:name run-name
             :snapshot snapshot-name
             :data (when (map? result) result)}
      (not (map? result))
      (assoc :error result))))


(defn process-info
  [{:keys [gene-sample-data, setup-data, run-name]}]
  (let [{:keys [iteration-count, population-size]} setup-data]
    {:name run-name
     :total-tasks (* iteration-count population-size)
     ; 2 runs remove-redundant-genes + 1 run to calculate objectives
     :difficulty (* 3 (gd/gene-count gene-sample-data) (gd/sample-count gene-sample-data))}))


(defn execute-batch
  [snapshot-list, {:keys [batch] :as batch-config}, create-progress-tracker]
  (let [{:keys [optimization-setup-list, thread-count], batch-name :name} batch,
        run-config-list (u/mapv-indexed
                          (fn [index, config]
                            (assoc config :index index))
                          (optimization-run-configs snapshot-list, batch-config, optimization-setup-list)),
        progress-tracker (create-progress-tracker (format "Executing batch \"%s\"" batch-name),
                           (mapv process-info run-config-list))
        result-list (cp/with-shutdown! [pool (cp/threadpool thread-count)]
                      (vec
                        (cp/pmap pool (partial run-optimization progress-tracker) run-config-list)))]
    {:name batch-name,
     :pareto-front-list result-list}))