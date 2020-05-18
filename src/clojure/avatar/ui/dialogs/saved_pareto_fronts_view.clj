; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.saved-pareto-fronts-view
  (:require
    [clj-jfx.core :as jfx]
    [avatar.data.management :as dm]
    [avatar.ui.tools :as t]
    [avatar.ui.dialogs.pareto-front-view :as pfv]
    [avatar.data.gene-data :as gd]
    [clj-jfx.selection :as jfx-select]
    [clj-jfx.properties :as props]
    [clojure.string :as str]))



(defn view-pareto-front
  [_, ui, data-mgmt, pareto-front-table]
  (let [pareto-front-pos (jfx/selected-row-data-index pareto-front-table)]
    (when (>= pareto-front-pos 0)
      (let [{:keys [name, data] :as pareto-front-data} (dm/saved-pareto-front data-mgmt, pareto-front-pos)]
        (pfv/pareto-front-view ui, (dm/plot-style data-mgmt), data, name)))))


(defn delete-pareto-fronts
  [_, data-mgmt, pareto-front-table]
  (let [data-indices (set (jfx/selected-row-data-indices pareto-front-table))]
    (when (pos? (count data-indices))
      (when (jfx/ask-confirmation "Delete Pareto front?",
              "Do you really want to delete the selected Pareto fronts?",
              "This can not be undone.")
        (dm/delete-saved-pareto-fronts data-mgmt, data-indices)))))


(defn create-dialog
  [ui]
  (jfx/run-now
    (let [data-mgmt (:data-management ui),
          pareto-front-data-ref (dm/pareto-front-data-ref data-mgmt),
          control (jfx/create-control "avatar/ui/SavedParetoFrontsView.fxml"),
          control-node (jfx/control-node control),
          {:keys [pareto-front-table,
                  view-pareto-front-button,
                  delete-pareto-front-button]
           :as children} (jfx/control-children control),
          window (doto (jfx/window "Saved Pareto Fronts", control-node)
                   (t/add-avatar-icon))]
      (jfx/setup-table-view! pareto-front-table,
        [:name
         [:data :parameters :alteration-type],
         [:data :parameters :second-objective],
         [:data :parameters :minimum-alteration-ratio],
         [:data :parameters :sample-group],
         [:data :hypervolume]]
        pareto-front-data-ref,
        :data-prefix [:single, :saved],
        :sort? true,
        :placeholder "There are no saved pareto fronts."
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
            (view-pareto-front e, ui, data-mgmt, pareto-front-table))))
      ; view
      (jfx/bind (props/property view-pareto-front-button :disable)
        (jfx/functional-property
          (fn [selected-indices]
            (not (= (count selected-indices) 1)))
          (jfx-select/observable-selected-indices pareto-front-table)))
      (jfx/handle-event! view-pareto-front-button :action view-pareto-front ui, data-mgmt, pareto-front-table)
      ; delete
      (jfx/bind (props/property delete-pareto-front-button :disable)
        (jfx/functional-property
          (fn [selected-indices]
            (not (pos? (count selected-indices))))
          (jfx-select/observable-selected-indices pareto-front-table)))
      (jfx/handle-event! delete-pareto-front-button :action delete-pareto-fronts data-mgmt, pareto-front-table)
      window)))