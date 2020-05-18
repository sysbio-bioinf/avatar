; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.import-table-file
  (:require [avatar.util :as u]
            [avatar.data.import :as imp]
            [clj-jfx.core :as jfx]
            [clj-jfx.properties :as props]
            [clj-jfx.table :as table]
            [clj-jfx.user-data :as ud]
            [avatar.ui.tools :as t]
            [avatar.ui.dialogs.import-matrix-file :as import-matrix-dialog]
            [clj-jfx.colors :as col]
            [clojure.string :as str])
  (:import (javafx.scene.layout Region)
           (javafx.geometry Pos)
           (clj_jfx.table CustomTableCell)
           (javafx.scene.control Label)))



(defn highlight-color
  [color-kw]
  (get
    {:header-row "#E7DA65"
     :sample-column "#77A8FF"
     :gene-column "#72C993"
     :other-columns "#AEFFFA"}
    color-kw))


(def ^:const maximal-cell-count 10000)

(defn display-row-count
  [{:keys [column-count, row-count] :as table-data}]
  (let [cell-count (* column-count row-count)]
    (if (> cell-count maximal-cell-count)
      (let [new-row-count (Math/round (/ (double maximal-cell-count) column-count))]
        [new-row-count,
         (format "Only a part of the table is shown (%.3f%%) since the complete table is too large."
           (* (/ (double new-row-count) row-count) 100))])
      [row-count])))


(defn index-chosen?
  [index]
  (boolean (and index (>= index 0))))


(defn content-cell-factory
  [dialog-data, _]
  (let [label (doto (Label.)
                (.setAlignment Pos/CENTER))
        cell (CustomTableCell. label, (props/property label, :text), str)
        position-prop (import-matrix-dialog/spreadsheet-cell-position-property cell)]
    (props/bind (props/property cell, :style)
      (props/fn-property
        (fn [{:keys [header-row-index, gene-column-index, sample-column-index, other-column-indices]}, {:keys [row, column] :as position}, value]
          (when (and (index-chosen? header-row-index) (>= row header-row-index))
            (when-let [cell-type (cond
                                   (and (index-chosen? gene-column-index) (= column gene-column-index))
                                   :gene-column

                                   (and (index-chosen? sample-column-index) (= column sample-column-index))
                                   :sample-column

                                   (and (seq other-column-indices) (contains? other-column-indices column))
                                   :other-columns

                                   (= row header-row-index)
                                   :header-row)]
              (format "-fx-background-color: %s" (highlight-color cell-type)))))
        (props/selected-keys-property dialog-data, #{:header-row-index, :gene-column-index, :sample-column-index, :other-column-indices})
        position-prop
        (props/property cell, :value)))
    (doto cell
      (-> .getStyleClass (.add "my-table-cell")))))


(defn create-table-data
  [dialog-data, note-label, {:keys [column-count, rows] :as selected-sheet}]
  (if selected-sheet
    (let [[row-count, note-message] (display-row-count selected-sheet)]
      (props/set-property note-label, :text, note-message)
      {:columns (into [{:attribute :row-index, :width 50, :cell-factory import-matrix-dialog/row-index-cell-factory}]
                  (mapv
                    (fn [column-index]
                      {:attribute column-index, :name (t/excel-letters column-index)
                       :min-width Region/USE_COMPUTED_SIZE, :max-width 300
                       :cell-factory (partial content-cell-factory dialog-data)
                       :font {:weight :bold}})
                    (range column-count)))
       :rows (mapv
               (fn [row-index]
                 (let [row (nth rows row-index)]
                   (merge {:row-index (inc row-index)} (zipmap (range column-count) row))))
               (range row-count))})
    {:columns [], :rows []}))


(defn bind-column-combobox-disable
  [header-row-index-prop, combobox]
  (props/bind (props/property combobox, :disable)
    (props/fn-property
      (fn [header-row-index]
        (not (index-chosen? header-row-index)))
      header-row-index-prop)))


(defn set-combobox-color
  [combobox, color]
  (doto combobox
    (props/set-property :style (format "-fx-base: %s;" (col/color->str color)))))


(defn setup-comboboxes
  [dialog-data, file-type, control-map]
  (let [{:keys [spreadsheet-combobox
                spreadsheet-label
                settings-grid
                header-row-combobox
                gene-column-combobox
                sample-column-combobox]} control-map
        selected-sheet-prop (props/entry-property dialog-data, [:selected-sheet])
        header-row-index-prop (props/entry-property dialog-data, [:header-row-index])
        column-names-prop (doto (props/entry-property dialog-data, [:column-names])
                            (props/bind
                              (props/fn-property
                                (fn [{:keys [rows] :as selected-sheet}, header-row-index]
                                  (if (index-chosen? header-row-index)
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
        (jfx/setup-combobox-first-selected! spreadsheet-combobox, (-> dialog-data props/get :sheets))
        (props/bind (props/entry-property dialog-data, [:sheet-index])
          (props/selected-index-property spreadsheet-combobox)))
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
            (let [[row-count] (display-row-count selected-sheet)]
              (mapv (comp str inc) (range row-count)))
            []))
        selected-sheet-prop)
      :initialize :list)

    (props/bind header-row-index-prop, (props/selected-index-property header-row-combobox))

    ; combobox colors
    (set-combobox-color header-row-combobox, (highlight-color :header-row))
    (set-combobox-color gene-column-combobox, (highlight-color :gene-column))
    (set-combobox-color sample-column-combobox, (highlight-color :sample-column))

    ; disable gene and sample column comboboxes when no header row is selected
    (bind-column-combobox-disable header-row-index-prop, gene-column-combobox)
    (bind-column-combobox-disable header-row-index-prop, sample-column-combobox)

    ; bind gene and sample column combobox items to column names in header row
    (props/list-property-binding (props/items gene-column-combobox), column-names-prop, :initialize :list)
    (props/list-property-binding (props/items sample-column-combobox), column-names-prop, :initialize :list)
    ; bind selected indices to dialog data
    (props/bind (props/entry-property dialog-data, [:gene-column-index])
      (props/selected-index-property gene-column-combobox))
    (props/bind (props/entry-property dialog-data, [:sample-column-index])
      (props/selected-index-property sample-column-combobox))))


(defn import-gene-sample-data
  [{:keys [selected-sheet, header-row-index, gene-column-index, sample-column-index] :as dialog-data}
   alteration-type
   file]
  (let [alteration-list (->> selected-sheet
                          :rows
                          ; drop all rows before the header and the header itself
                          (drop (inc header-row-index))
                          (mapv
                            (fn [row]
                              {:gene (nth row gene-column-index), :sample (nth row sample-column-index)})))]
    (imp/alteration-list->gene-sample-data alteration-type, (u/file-name file), alteration-list)))


(defn show-dialog
  [ui, file, completion-fn]
  (let [file-type (u/import-file-type file)]
    (if-let [data (case file-type
                    :csv (imp/import-csv-file file, false),
                    :excel (imp/import-excel-file file))]
      (jfx/run-now
        (let [dialog-data-atom (atom {:sheets (when (= file-type :excel)
                                                (mapv :sheet-name (:sheets data)))})
              dialog-data (props/data-property dialog-data-atom)
              control (jfx/create-control "avatar/ui/ImportTableDialog.fxml"),
              control-node (jfx/control-node control),
              {:keys [data-table
                      note-label
                      import-button,
                      cancel-button] :as control-map} (jfx/control-children control),
              window (doto (jfx/modal-window
                             (format "Import data from %s file" (case file-type :csv "CSV" :excel "Excel"))
                             control-node)
                       (t/add-avatar-icon))
              selected-sheet-prop (props/entry-property dialog-data, [:selected-sheet])]

          #_(t/watch-property dialog-data)

          (table/setup-table data-table, [], {})

          (props/listen-to
            (fn [_, [selected-sheet]]
              (props/set (table/data-property data-table) (create-table-data dialog-data, note-label, selected-sheet)))
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


          (props/bind (props/property import-button, :disable)
            (props/fn-property
              (fn [{:keys [header-row-index, gene-column-index, sample-column-index]}]
                (or
                  (not (index-chosen? header-row-index))
                  (not (index-chosen? gene-column-index))
                  (not (index-chosen? sample-column-index))
                  (= gene-column-index sample-column-index)))
              (props/selected-keys-property dialog-data, #{:header-row-index, :gene-column-index, :sample-column-index})))

          (jfx/handle-event! import-button, :action,
            (fn [_]
              (let [alteration-type (t/ask-for-alteration-type #{:mutation-data, :expression-data, :methylation-data},
                                      "Specify type of alteration data"
                                      "Which kind of alteration data are you importing?")
                    gene-sample-data (import-gene-sample-data (props/get dialog-data), alteration-type, file)]
                (jfx/close window)
                (completion-fn gene-sample-data))))

          (jfx/handle-event! cancel-button, :action,
            (fn [_]
              (jfx/close window)))

          (jfx/show window)))
      ; could not import data
      (do
        (jfx/show-error "Import Error",
          "Could not import alteration table."
          (format "The %s file \"%s\" does not contain a valid alteration table!"
            (name file-type), file))
        ; return nil
        nil))))