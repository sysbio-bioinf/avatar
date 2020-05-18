; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.gene-symbol-normalization
  (:require [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [clojure.string :as str]
            [avatar.ui.tools :as t])
  (:import (clj_jfx.table ComboBoxSelectionTableCell)
           (javafx.scene.control CheckBox Button TextField)
           (javafx.scene.text Text TextFlow)))



(defn normalization-candidates
  [{:keys [symbol->synonyms, synonym->symbols]}, gene-list]
  (->> gene-list
    (map #(update-in % [:gene] str/trim))
    ; remove genes with approved gene symbol
    (remove
      (fn [{:keys [gene]}]
        (contains? symbol->synonyms gene)))
    ; build vector of non approved symbols that are synonym to another symbol
    (reduce
      (fn [result-vec, {:keys [gene]}]
        (if-let [synonym-set (get synonym->symbols gene)]
          (conj! result-vec {:synonym gene, :gene-selection {:rename? false, :gene-set (->> synonym-set sort vec), :selected-gene -1}})
          result-vec))
      (transient []))
    persistent!))



(defn combobox-selection-cell
  []
  (doto ^ComboBoxSelectionTableCell (jfx/combobox-selection-cell :gene-set, :selected-gene, :selection-prompt "Select gene symbol")
    (.setSelectionEnabledKey :rename?)))


(defn selected-rows
  [table-rows]
  (filterv #(get-in % [:gene-selection, :rename?]) table-rows))


(defn valid-row?
  [{:keys [gene-selection]}]
  (let [{:keys [rename?, selected-gene, gene-set]} gene-selection]
    (or
      (not rename?)
      (= (count gene-set) 1)
      (>= selected-gene 0))))


(defn show-dialog
  [symbol-synonym-data, gene-list, complete-fn]
  (jfx/run-now
    (if-let [not-approved-synonyms (seq (normalization-candidates symbol-synonym-data, gene-list))]
      (let [control (jfx/create-control "avatar/ui/GeneSymbolNormalizationDialog.fxml"),
            control-node (jfx/control-node control),
            {:keys [^CheckBox select-all-checkbox
                    renaming-table
                    ^Text
                    selected-renaming-count-text
                    ^Text
                    total-renaming-count-text
                    ^TextFlow
                    error-textflow
                    ^Text
                    error-synonym-count-text
                    ^Text
                    error-synonyms-text
                    ^TextField
                    filter-textfield
                    ^Button
                    apply-button
                    cancel-button]} (jfx/control-children control)

            table-data (atom (vec (sort-by :synonym not-approved-synonyms))),
            table-rows-prop (jfx/->property table-data),
            errors-prop (jfx/functional-property
                          (fn [table-rows]
                            (vec
                              (keep
                                (fn [{:keys [synonym] :as row}]
                                  (when-not (valid-row? row)
                                    synonym))
                                table-rows)))
                          table-rows-prop)
            window (doto (jfx/modal-window "Normalize Gene Symbols", control-node)
                     (t/add-avatar-icon))]

        (jfx/setup-table-view! renaming-table, [[:gene-selection, :rename?], :synonym, :gene-selection], table-data,
          :filter-text-property (jfx/property filter-textfield, :text),
          :column-cell-factories
          {:rename? (fn [] (jfx/checkbox-cell))
           :gene-selection combobox-selection-cell})

        ; total number of renaming options
        (jfx/bind (jfx/property total-renaming-count-text, :text),
          (jfx/functional-property
            (fn [table-rows]
              (str (count table-rows)))
            table-rows-prop))

        ; count selected gene symbols to rename
        (jfx/bind (jfx/property selected-renaming-count-text, :text),
          (jfx/functional-property
            (fn [table-rows]
              (str (count (selected-rows table-rows))))
            table-rows-prop))

        (jfx/listen-to
          (fn [selected?]
            (swap! table-data
              (fn [table-rows]
                (mapv
                  #(assoc-in % [:gene-selection, :rename?] selected?)
                  table-rows))))
          (jfx/property select-all-checkbox, :selected))


        (jfx/change-listener! table-rows-prop
          (fn [_, old-rows, new-rows]
            (when (jfx/property-value select-all-checkbox, :selected)
              (let [all-selected? (every? #(get-in % [:gene-selection, :rename?]) new-rows)]
                (jfx/property-value! select-all-checkbox, :indeterminate, (not all-selected?))))))


        (jfx/bind (jfx/property apply-button, :disable)
          (jfx/functional-property
            (fn [table-rows]
              (let [selected-rows (selected-rows table-rows)]
                (not
                  (and
                    (seq selected-rows)
                    (every?
                      valid-row?
                      selected-rows)))))
            table-rows-prop))

        ; setup error information display

        (jfx/bind (jfx/property error-textflow, :visible)
          (jfx/functional-property
            (fn [errors]
              (boolean (seq errors)))
            errors-prop))

        (jfx/bind (jfx/property error-synonym-count-text, :text)
          (jfx/functional-property
            (fn [errors]
              (str (count errors)))
            errors-prop))

        (jfx/bind (jfx/property error-synonyms-text, :text)
          (jfx/functional-property
            (fn [errors]
              (cond-> (->> errors
                        (take 20)
                        sort
                        (str/join ", "))
                (> (count errors) 20)
                (str " ...")))
            errors-prop))


        (jfx/handle-event! apply-button, :action
          (fn [_]
            (let [renaming-map (->> table-data
                                 deref
                                 ; get synonyms to rename
                                 (filterv #(get-in % [:gene-selection, :rename?]))
                                 ; build renaming map
                                 (reduce
                                   (fn [result-map, {:keys [synonym, gene-selection]}]
                                     (let [{:keys [gene-set, selected-gene]} gene-selection,
                                           symbol (if (= (count gene-set) 1)
                                                    (first gene-set)
                                                    (nth gene-set selected-gene))]
                                       (assoc! result-map synonym symbol)))
                                   (transient {}))
                                 persistent!)]
              (complete-fn renaming-map))
            (jfx/close window)))


        (jfx/handle-event! cancel-button, :action
          (fn [_]
            (jfx/close window)))


        #_(add-watch table-data :debug
            (fn [_, _, old-state, new-state]
              (u/print-modifications old-state, new-state)))

        (jfx/show window))
      (jfx/show-information "Normalize Gene Names",
        "No renaming required."
        "The current data does not contain not-approved gene symbols that can be renamed to approved symbols."))))