; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.merge-group
  (:require [clj-jfx.core :as jfx]
            [avatar.ui.plot :as plot]
            [avatar.algorithms.column-sort :as col-sort]
            [clojure.data.int-map :as im]
            [avatar.ui.plot-export :as pexp]
            [avatar.ui.dialogs.statistics-view :as sv]
            [avatar.ui.tools :as t])
  (:import
    (javafx.scene.control SingleSelectionModel ComboBox)))





(defn create-empty-plot
  [data-mgmt, parent-control, empty-group-data, plot-style, plot, export?]
  (let [[width, height] (jfx/layout-bounds parent-control),
        plot (plot/create-plot empty-group-data, plot-style, width, height,
               (cond-> {:plot-ref plot, :show-header? false}
                 export?
                 (assoc
                   :snapshot-handler (partial pexp/save-plot-as-snapshot data-mgmt),
                   :show-objectives-handler (partial sv/show-objectives-view data-mgmt))))]
    (jfx/add-child parent-control, (plot/root-control plot))))


(defn merge-sets
  [merge-fn, alteration-type, data-type, gene-list]
  (reduce
    merge-fn
    (mapv #(get-in % [:alteration-data, alteration-type, data-type]) gene-list)))


(defn merged-gene
  [alteration-types, group-name, group-genes]
  {:gene group-name,
   :group nil,
   ; grab smallest gene-id
   :gene-id (reduce min (mapv :gene-id group-genes)),
   ; NOTE: missing values remain only if every gene has a missing value for that sample
   :alteration-data (persistent!
                      (reduce
                        (fn [result-map, alteration-type]
                          (assoc! result-map
                            alteration-type
                            {:alterations (merge-sets im/union, alteration-type, :alterations, group-genes),
                             :missing-values (merge-sets im/intersection, alteration-type, :missing-values, group-genes)}))
                        (transient {})
                        alteration-types))
   :order-fixed? false})


(defn show-dialog
  [data-mgmt, {:keys [gene-list, alteration-type-set] :as gene-sample-data}, {:keys [shown-alteration-type] :as plot-style}]
  (let [groups (->> gene-list
                 (keep :group)
                 distinct
                 sort)]
    (if (empty? groups)
      (do
        (jfx/show-information "Merge Genes in Group", "There are no gene groups in the current data.", nil)
        nil)
      (let [empty-group-data (assoc gene-sample-data :gene-list [])
            dialog-data (atom {:dialog-result nil,
                               :group-genes nil,
                               :result-gene nil,
                               :selected-group-index 0,
                               :group-data empty-group-data})
            control (jfx/create-control "avatar/ui/MergeGroupDialog.fxml"),
            control-node (jfx/control-node control),
            {:keys [^ComboBox
            group-combobox,
                    group-plot-stackpane,
                    result-plot-stackpane,
                    alteration-type-combobox,
                    merge-group-button,
                    cancel-button]} (jfx/control-children control),
            group-plot (ref nil),
            result-plot (ref nil),
            selected-group-index-prop (jfx/map-entry-property dialog-data, [:selected-group-index]),
            ^SingleSelectionModel selection-model (jfx/property-value group-combobox, :selection-model),
            window (-> (jfx/modal-window "Create a Gene Group", control-node, 1000, 400)
                     (jfx/resizable! false)
                     (t/add-avatar-icon))
            ; setup alteration type choice
            alteration-type-prop (t/setup-alteration-type-combobox alteration-type-combobox, alteration-type-set,
                                   :selected-alteration-type shown-alteration-type)]
        ; set groups
        (jfx/property-value! group-combobox, :items (jfx/observable-array-list groups))
        ; sync index with combobox
        (jfx/bind selected-group-index-prop (jfx/property selection-model, :selected-index))

        (jfx/listen-to
          (fn [selected-group-index, alteration-type]
            (let [group-name (nth groups selected-group-index)
                  group-genes (filterv #(= (:group %) group-name) gene-list),
                  result-gene (merged-gene alteration-type-set, group-name, group-genes),
                  group-gene-sample-data (-> empty-group-data
                                           (assoc :gene-list group-genes)
                                           (->> (col-sort/lexicographic-sort true, false, alteration-type))),
                  plot-style (assoc plot-style :shown-alteration-type alteration-type)]
              (plot/update-data! group-plot,
                group-gene-sample-data
                plot-style)
              (plot/update-data! result-plot,
                (assoc empty-group-data
                  :gene-list [result-gene]
                  :sample-permutation (:sample-permutation group-gene-sample-data))
                plot-style)
              (swap! dialog-data assoc
                :group-genes group-genes,
                :result-gene result-gene)))
          selected-group-index-prop
          alteration-type-prop)

        (jfx/handle-event! merge-group-button, :action,
          (fn [_]
            (swap! dialog-data
              (fn [dialog-data-map]
                (assoc dialog-data-map :dialog-result (select-keys dialog-data-map [:group-genes, :result-gene]))))
            (jfx/close window)))

        (jfx/handle-event! cancel-button, :action,
          (fn [_]
            (jfx/close window)))

        (jfx/handle-event! window, :window-shown
          (fn [_]
            (create-empty-plot data-mgmt, group-plot-stackpane, empty-group-data, plot-style, group-plot, true)
            (create-empty-plot data-mgmt, result-plot-stackpane, empty-group-data, plot-style, result-plot, false)
            (.select selection-model 0)))

        (jfx/show-and-wait window)
        (:dialog-result (deref dialog-data))))))