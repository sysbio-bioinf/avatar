; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.group-samples
  (:require
    [clj-jfx.core :as jfx]
    [avatar.util :as u]
    [clojure.string :as str]
    [avatar.algorithms.common :as co]
    [clj-jfx.selection :as jfx-select]))


(defn collect-attribute-values
  [selected-attribute, clinical-data-map]
  (let [frequency-map (persistent!
                        (reduce-kv
                          (fn [frequency-map, _, sample-data]
                            (let [value (co/attribute-value sample-data, selected-attribute)]
                              (u/update-in! frequency-map [value] (fnil inc 0))))
                          (transient {})
                          clinical-data-map))]
    (persistent!
      (reduce-kv
        (fn [result-vec, attribute-value, frequency]
          (conj! result-vec {:value attribute-value, :frequency frequency}))
        (transient [])
        frequency-map))))


(defn show-dialog
  [{:keys [clinical-attributes, clinical-data-map] :as gene-sample-data}, completion-fn]
  (if (empty? clinical-data-map)
    (jfx/show-information "Group samples by clinical data",
      "The sample groups cannot be grouped by clinical data!",
      "The current dataset does not include any clinical data.")
    (let [dialog-data (atom {:selected-attribute nil})
          table-data (atom [])
          control (jfx/create-control "avatar/ui/GroupSamplesDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [attribute-value-combobox,
                  clinical-attribute-table,
                  apply-button,
                  cancel-button]
           :as children} (jfx/control-children control)
          window (jfx/modal-window "Group samples by clinical attribute", control-node)
          selected-attribute-property (jfx/map-entry-property dialog-data [:selected-attribute])]
      (jfx/setup-combobox-unselected! attribute-value-combobox,
        (sort-by str/upper-case (disj clinical-attributes "SAMPLE_ID", "PATIENT_ID", "OTHER_SAMPLE_ID", "OTHER_PATIENT_ID")),
        "Select clinical attribute")
      (jfx/bind selected-attribute-property, (jfx-select/selected-item-property attribute-value-combobox))
      (jfx/setup-table-view! clinical-attribute-table, [:value, :frequency], table-data, :sort? true)
      (jfx/listen-to
        (fn [selected-attribute]
          (reset! table-data (vec (sort-by :frequency > (collect-attribute-values selected-attribute, clinical-data-map)))))
        selected-attribute-property)

      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (completion-fn (jfx/value selected-attribute-property))
          (jfx/close window)))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      #_(add-watch dialog-data :debug
          (fn [_, _, old, new]
            (u/print-modifications old, new)))

      (jfx/show window))))