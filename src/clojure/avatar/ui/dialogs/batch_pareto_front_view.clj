; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.batch-pareto-front-view
  (:require [clj-jfx.core :as jfx]
            [avatar.data.management :as dm]
            [avatar.data.gene-data :as gd]
            [avatar.util :as u]
            [avatar.ui.dialogs.pareto-front-view :as pfv]
            [clj-jfx.selection :as jfx-select]
            [clojure.string :as str]
            [clj-jfx.properties :as props]
            [avatar.ui.tools :as t]))




(defn view-pareto-front
  [_, ui, data-mgmt, table-data, pareto-front-table]
  (let [selected-index (jfx/selected-row-data-index pareto-front-table)]
    (when (>= selected-index 0)
      (let [{:keys [batch-index, front-index]} (nth (deref table-data) selected-index)
            {:keys [name, data]} (dm/batch-pareto-front data-mgmt, batch-index, front-index)]
        (pfv/pareto-front-view ui, (dm/plot-style data-mgmt), data, (str/join " / " name))))))


(defn delete-pareto-fronts
  [_, data-mgmt, table-data, pareto-front-table]
  (let [data-indices (set (jfx/selected-row-data-indices pareto-front-table))]
    (when (pos? (count data-indices))
      (when (jfx/ask-confirmation "Delete Pareto front?",
              "Do you really want to delete the selected Pareto fronts?",
              "This can not be undone.")
        (->> data-indices
          (mapv (partial nth (deref table-data)))
          (group-by :batch-index)
          (u/for-each-kv!
            (fn [batch-index, group]
              (let [front-indices (into #{} (map :front-index) group)]
                (dm/delete-batch-pareto-fronts data-mgmt, batch-index, front-indices)))))))))


(defn extract-pareto-fronts
  [batch-index, {:keys [name, pareto-front-list]}]
  (eduction
    (keep-indexed
      (fn [front-index, {:keys [snapshot, data, error]}]
        (when-not error
          (let [{:keys [hypervolume, parameters]} data]
            (merge
              {:batch name,
               :batch-index batch-index
               :front-index front-index
               :snapshot snapshot
               :hypervolume hypervolume}
              (select-keys parameters [:alteration-type, :second-objective, :minimum-alteration-ratio, :sample-group]))))))
    pareto-front-list))


(defn batch-pareto-fronts
  [batch-list]
  (into []
    (comp
      (map-indexed extract-pareto-fronts)
      cat)
    batch-list))


(defn create-dialog
  [ui]
  (jfx/run-now
    (let [data-mgmt (:data-management ui),
          pareto-front-data-ref (dm/pareto-front-data-ref data-mgmt),
          control (jfx/create-control "avatar/ui/BatchParetoFrontsView.fxml"),
          control-node (jfx/control-node control),
          {:keys [pareto-front-table,
                  view-pareto-front-button,
                  delete-pareto-front-button]
           :as children} (jfx/control-children control),
          table-data (atom (batch-pareto-fronts (:batch (deref pareto-front-data-ref))))
          window (doto (jfx/window "Batch Optimization Pareto Fronts", control-node)
                   (t/add-avatar-icon))]

      (add-watch pareto-front-data-ref :watch-batch-pareto-fronts
        (fn [_, _, {old-batches :batch}, {new-batches :batch}]
          (when-not (= old-batches new-batches)
            (reset! table-data (batch-pareto-fronts new-batches)))))

      (jfx/setup-table-view! pareto-front-table,
        [:batch :snapshot :alteration-type, :second-objective, :minimum-alteration-ratio, :sample-group, :hypervolume]
        table-data,
        :sort? true,
        :placeholder "There are no batch pareto fronts."
        :column-cell-value-fns {:second-objective (fn [second-objective]
                                                    (case second-objective
                                                      :overlap "overlap"
                                                      :gene-count "gene count")),
                                :minimum-alteration-ratio #(format "%.2f" %),
                                :alteration-type gd/alteration-type-str,
                                :hypervolume (fn [hypervolume] (format "%.6f" hypervolume))})
      (jfx-select/enable-multiple-selection! pareto-front-table)
      (jfx/handle-event! pareto-front-table, :mouse-clicked,
        (fn [e]
          (when (jfx/double-click? e)
            (view-pareto-front e, ui, data-mgmt, table-data, pareto-front-table))))
      ; view
      (jfx/bind (props/property view-pareto-front-button :disable)
        (jfx/functional-property
          (fn [selected-indices]
            (not (= (count selected-indices) 1)))
          (jfx-select/observable-selected-indices pareto-front-table)))
      (jfx/handle-event! view-pareto-front-button :action view-pareto-front ui, data-mgmt, table-data, pareto-front-table)
      ; delete
      (jfx/bind (props/property delete-pareto-front-button :disable)
        (jfx/functional-property
          (fn [selected-indices]
            (not (pos? (count selected-indices))))
          (jfx-select/observable-selected-indices pareto-front-table)))
      (jfx/handle-event! delete-pareto-front-button :action delete-pareto-fronts data-mgmt, table-data, pareto-front-table)
      window)))