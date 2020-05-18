; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.import-clinical-data
  (:require [clj-jfx.core :as jfx]
            [avatar.data.import :as imp]
            [avatar.util :as u]
            [clj-jfx.properties :as props]
            [clj-jfx.user-data :as ud]
            [avatar.ui.dialogs.import-table-file :as itd]
            [avatar.ui.dialogs.import-matrix-file :as import-matrix-dialog]
            [avatar.ui.tools :as t]
            [clj-jfx.table :as table]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (javafx.scene.layout Region)
           (clj_jfx.table CustomTableCell)
           (javafx.geometry Pos)))





(defn setup-comboboxes
  [dialog-data, file-type, control-map]
  (let [{:keys [settings-grid
                spreadsheet-label
                spreadsheet-combobox
                header-row-combobox
                sample-column-combobox]} control-map
        selected-sheet-prop (props/entry-property dialog-data, [:selected-sheet])
        header-row-index-prop (props/entry-property dialog-data, [:header-row-index])
        column-names-prop (doto (props/entry-property dialog-data, [:column-names])
                            (props/bind
                              (props/fn-property
                                (fn [{:keys [rows] :as selected-sheet}, header-row-index]
                                  (if (itd/index-chosen? header-row-index)
                                    (let [header-row (nth rows header-row-index)]
                                      (mapv
                                        (fn [column-index, column-value]
                                          (if (str/blank? column-value)
                                            (format "\"%s\"" (t/excel-letters column-index))
                                            (str column-value)))
                                        (range) header-row))
                                    []))
                                selected-sheet-prop
                                header-row-index-prop)))]
    (if (= file-type :excel)
      (do
        (do
          (jfx/setup-combobox-first-selected! spreadsheet-combobox, (-> dialog-data props/get :sheets))
          (props/bind (props/entry-property dialog-data, [:sheet-index])
            (props/selected-index-property spreadsheet-combobox))))
      (do
        (jfx/remove-children settings-grid, spreadsheet-label, spreadsheet-combobox)
        (jfx/update-grid-constraint (nth (jfx/grid-constraints settings-grid, :row) 0), {:min-length 0.0, :max-length 0.0})))

    (props/list-property-binding (props/items header-row-combobox)
      (props/fn-property
        (fn [{:keys [row-count] :as selected-sheet}]
          (if selected-sheet
            (mapv str (range row-count))
            []))
        selected-sheet-prop)
      :initialize :list)

    (props/list-property-binding (props/items header-row-combobox)
      (props/fn-property
        (fn [selected-sheet]
          (if selected-sheet
            (let [[row-count] (itd/display-row-count selected-sheet)]
              (mapv (comp str inc) (range row-count)))
            []))
        selected-sheet-prop)
      :initialize :list)

    (props/bind header-row-index-prop, (props/selected-index-property header-row-combobox))

    ; combobox colors
    (itd/set-combobox-color header-row-combobox, (itd/highlight-color :header-row))
    (itd/set-combobox-color sample-column-combobox, (itd/highlight-color :sample-column))

    ; disable gene and sample column comboboxes when no header row is selected
    (itd/bind-column-combobox-disable header-row-index-prop, sample-column-combobox)

    ; bind gene and sample column combobox items to column names in header row
    (props/list-property-binding (props/items sample-column-combobox), column-names-prop, :initialize :list)
    ; bind selected indices to dialog data
    (props/bind (props/entry-property dialog-data, [:sample-column-index])
      (props/selected-index-property sample-column-combobox))
    ))


(defn extract-attribute-columns
  [rows, {:keys [header-row-index, sample-column-index]}]
  (if (and rows (seq rows) (itd/index-chosen? header-row-index) (itd/index-chosen? sample-column-index))
    (let [header-row (nth rows header-row-index)]
      (persistent!
        (reduce-kv
          (fn [column-vec, column-index, column-value]
            (let [column-name (if (and (string? column-value) (str/blank? column-value))
                                (format "\"%s\"" (t/excel-letters column-index))
                                (str column-value))]
              (cond-> column-vec
                (not= column-index sample-column-index)
                (conj!
                  {:column-name column-name
                   :column-index column-index
                   :import? false}))))
          (transient [])
          (into (sorted-map) (dissoc header-row :row-index)))))
    []))


(defn collect-sample-data
  [column->index-map, row]
  (persistent!
    (reduce-kv
      (fn [sample-data-map, column, index]
        (assoc! sample-data-map
          column (when (< index (count row)) (nth row index))))
      (transient {})
      column->index-map)))


(defn collect-clinical-data
  [{:keys [header-row-index, sample-column-index, other-column-indices, selected-sheet] :as import-config}]
  (let [rows (drop header-row-index (:rows selected-sheet)),
        columns-row (first rows),
        column->index-map (persistent!
                            (reduce
                              (fn [result-map, index]
                                (assoc! result-map (nth columns-row index) index))
                              (transient {})
                              other-column-indices))]
    {:clinical-attributes (set (keys column->index-map))
     :clinical-data-map (persistent!
                          (reduce
                            (fn [result-map, row]
                              (let [sample-id (nth row sample-column-index)
                                    sample-data (collect-sample-data column->index-map, row)]
                                (assoc! result-map sample-id sample-data)))
                            (transient {})
                            (rest rows)))}))


(defn check-for-missing-samples
  [data-sample-id-set, {:keys [header-row-index, sample-column-index, selected-sheet]}]
  (when (and sample-column-index (>= sample-column-index 0))
    (let [table-sample-id-set (persistent!
                                (transduce
                                  (comp
                                    (drop (inc header-row-index))
                                    (map #(str/trim (nth % sample-column-index))))
                                  conj!
                                  (transient #{})
                                  (:rows selected-sheet))),
          missing-sample-ids (set/difference data-sample-id-set table-sample-id-set)]
      (when (seq missing-sample-ids)
        (format "WARNING: %d sample ids are missing in this table. (Did you select the correct SAMPLE ID column?)" (count missing-sample-ids))))))


(defn show-dialog
  [ui, file, sample-id-set, complete-fn]
  (let [file-type (u/import-file-type file)]
    (if-let [data (case file-type
                    :csv (imp/import-csv-file file, false)
                    :excel (imp/import-excel-file file))]
      (jfx/run-now
        (let [dialog-data-atom (atom {:sheets (when (= file-type :excel)
                                                (mapv :sheet-name (:sheets data)))})
              dialog-data (props/data-property dialog-data-atom)
              control (jfx/create-control "avatar/ui/ImportClinicalDataDialog.fxml")
              control-node (jfx/control-node control)
              {:keys [data-table
                      note-label
                      clinical-attribute-table
                      select-all-checkbox
                      sample-column-error-label
                      import-button
                      cancel-button]
               :as control-map} (jfx/control-children control)
              window (doto (jfx/modal-window
                             (format "Import clinical data from %s file" (case file-type :csv "CSV" :excel "Excel"))
                             control-node)
                       (t/add-avatar-icon))
              selected-sheet-prop (props/entry-property dialog-data, [:selected-sheet])
              attribute-columns-prop (props/entry-property dialog-data, [:attribute-columns])
              other-column-indices-prop (props/entry-property dialog-data, [:other-column-indices])]

          (table/setup-table data-table, [], {})
          (table/setup-table clinical-attribute-table,
            [{:attribute :import?
              :sortable? true
              :editable? true
              :width 30
              :cell-factory (fn [_] (jfx/checkbox-cell))}
             {:attribute :column-name
              :sortable? true
              :name "Attribute Column"
              :font {:weight :bold}
              :min-width Region/USE_COMPUTED_SIZE
              :max-width Double/MAX_VALUE}]
            {:sort? true})

          (props/listen-to
            (fn [_, [selected-sheet]]
              (props/set (table/data-property data-table)
                (itd/create-table-data dialog-data, note-label, selected-sheet)))
            selected-sheet-prop)

          (setup-comboboxes dialog-data, file-type, control-map)

          ; bind selected sheet
          (props/bind selected-sheet-prop
            (if (= file-type :excel)
              ; excel: data of selected sheet
              (props/fn-property
                (fn [sheet-index]
                  (when (and sheet-index (<= 0 sheet-index))
                    (get-in data [:sheets, sheet-index])))
                (props/entry-property dialog-data, [:sheet-index]))
              ; csv: just the data
              (jfx/object-property data)))

          ; extract attribute columns from table based on select header row and sample column
          (props/listen-to
            (fn [_, [rows, indices-map]]
              (props/set attribute-columns-prop, (extract-attribute-columns rows, indices-map)))
            (table/rows-property data-table)
            (props/selected-keys-property dialog-data, #{:header-row-index, :sample-column-index}))

          (props/bind-bidi (table/rows-property clinical-attribute-table), attribute-columns-prop)

          ; bind :other-column-indices
          (props/bind other-column-indices-prop
            (props/fn-property
              (fn [rows]
                (persistent!
                  (reduce
                    (fn [selected-column-set, {:keys [import?, column-index]}]
                      (cond-> selected-column-set import? (conj! column-index)))
                    (transient #{})
                    rows)))
              attribute-columns-prop))

          ; setup select-all-checkbox: select or deselect all attribute columns
          (props/listen-to
            (fn [_, [selected?]]
              (props/swap attribute-columns-prop
                (fn [columns]
                  (mapv
                    #(assoc % :import? selected?)
                    columns))))
            (props/property select-all-checkbox, :selected))

          ; set select-all-checkbox to indeterminate if some attribute columns were deselected
          (props/listen-to
            (fn [_, [attribute-columns]]
              (when (props/get-property select-all-checkbox, :selected)
                (let [all-selected? (every? :import? attribute-columns)]
                  (props/set-property select-all-checkbox, :indeterminate, (not all-selected?)))))
            attribute-columns-prop)


          (props/bind (props/property sample-column-error-label, :text)
            (props/fn-property
              (partial check-for-missing-samples sample-id-set)
              (props/selected-keys-property dialog-data, #{:header-row-index, :sample-column-index, :selected-sheet})))

          ; disable import button until the setup is completely specified
          (props/bind (props/property import-button, :disable)
            (props/fn-property
              (fn [{:keys [header-row-index, sample-column-index, other-column-indices]}]
                (or
                  (not (itd/index-chosen? header-row-index))
                  (not (itd/index-chosen? sample-column-index))
                  (empty? other-column-indices)))
              (props/selected-keys-property dialog-data, #{:header-row-index, :sample-column-index, :other-column-indices})))

          (jfx/handle-event! import-button, :action,
            (fn [_]
              (let [import-config (props/get dialog-data),
                    clinical-data (collect-clinical-data import-config)]
                (jfx/close window)
                (complete-fn clinical-data))))


          (jfx/handle-event! cancel-button, :action,
            (fn [_]
              (jfx/close window)))

          (jfx/show window)))
      ; could not import data
      (do
        (jfx/show-error "Import Error",
          "Could not import clinical attribute."
          (format "The %s file \"%s\" does not contain a valid clinical data table!"
            (name file-type), file))
        ; return nil
        nil))))