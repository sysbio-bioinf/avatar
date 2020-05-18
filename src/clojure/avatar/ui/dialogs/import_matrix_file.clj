; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.import-matrix-file
  (:require
    [clj-jfx.core :as jfx]
    [avatar.data.import :as imp]
    [avatar.util :as u]
    [clojure.string :as str]
    [clj-jfx.properties :as props]
    [avatar.ui.tools :as t]
    [clj-jfx.table :as table]
    [clj-jfx.user-data :as ud]
    [clj-jfx.colors :as col]
    [clojure.data :as d])
  (:import (clj_jfx.table CustomTableCell RadioButtonTableCell)
           (javafx.scene.control Label TableColumn RadioButton TableRow ToggleGroup)
           (javafx.geometry Pos)
           (javafx.scene.layout Region)))



(defonce cell-colors
  {:sample-id "#77A8FF",
   :sample-group "#BBD3FF"
   :gene-name "#72C993"
   :gene-group "#ADF5C5"
   :overlap "darkgray"
   :alteration "#C0B33B"
   :wild-type "#EEE9BF"
   :missing "#EE2C2C"})


(defn int->layout
  [^long layout]
  (case layout
    0 :samples-in-rows
    1 :samples-in-columns))


(defn normalize-marker
  [marker]
  (let [marker (some-> marker str/trim str/upper-case)]
    (when-not (str/blank? marker)
      marker)))


(defn normalize-rows
  [{:keys [rows] :as table-data}]
  (assoc table-data
    :rows
    (mapv
      (fn [row]
        (mapv normalize-marker row))
      rows)))


(defn setup-checkbox-combobox-pair!
  [dialog-data, config-kw, checkbox, combobox]
  (let [checkbox-selected-prop (props/property checkbox :selected)]
    ; enable combobox iff checkbox selected
    (props/bind (props/property combobox, :disable),
      (props/fn-property not checkbox-selected-prop))
    ; bind selected combobox index (if enabled) to config data
    (props/bind (props/entry-property dialog-data, [:layout, config-kw])
      (props/fn-property
        (fn [selected?, item]
          (when selected? item))
        checkbox-selected-prop
        (props/selected-index-property combobox)))))


(defn fill-layout-combobox
  [dialog-data, type, combobox]
  (props/list-property-binding (props/items combobox),
    (props/fn-property
      (fn [column-names, row-names, layout-type]
        (case layout-type
          :samples-in-rows (case type
                             :genes row-names
                             :samples column-names)
          :samples-in-columns (case type
                                :genes column-names
                                :samples row-names)))
      (props/entry-property dialog-data, [:column-names])
      (props/entry-property dialog-data, [:row-names])
      (props/entry-property dialog-data, [:layout-type]))
    :initialize :list))


(defn setup-comboboxes
  [dialog-data, file-type, control-map]
  (let [{:keys [gene-groups-checkbox, gene-groups-combobox
                sample-ids-checkbox, sample-ids-combobox
                sample-groups-checkbox, sample-groups-combobox
                gene-names-combobox
                spreadsheet-combobox
                data-layout-combobox]} control-map
        layout-type-prop (props/entry-property dialog-data, [:layout-type])]
    ; setup pairs of checkbox and combobox. bind the selected combobox values to :config
    (setup-checkbox-combobox-pair! dialog-data, :gene-groups-index, gene-groups-checkbox, gene-groups-combobox)
    (setup-checkbox-combobox-pair! dialog-data, :sample-ids-index, sample-ids-checkbox, sample-ids-combobox)
    (setup-checkbox-combobox-pair! dialog-data, :sample-groups-index, sample-groups-checkbox, sample-groups-combobox)
    ; color comboboxes
    (doseq [[combobox, attribute] {gene-names-combobox :gene-name,
                                   gene-groups-combobox :gene-group,
                                   sample-ids-combobox :sample-id,
                                   sample-groups-combobox :sample-group}]
      (props/set-property combobox, :style, (format "-fx-base: %s;" (get cell-colors attribute))))
    ; bind gene names selection
    (props/bind (props/entry-property dialog-data, [:layout, :gene-names-index])
      (props/fn-property
        (fn [index]
          (when (<= 0 index) index))
        (props/selected-index-property gene-names-combobox)))
    ; data layout
    (jfx/setup-combobox-first-selected! data-layout-combobox, ["Samples in Rows", "Samples in Columns"])
    (props/bind layout-type-prop
      (props/fn-property int->layout (props/selected-index-property data-layout-combobox)))
    (props/listen-to
      (fn [_, _]
        (doseq [combobox [gene-names-combobox, gene-groups-combobox, sample-ids-combobox, sample-groups-combobox]]
          (props/clear-selection combobox)))
      layout-type-prop)
    ; if Excel file, then support multiple sheets
    (if (= file-type :excel)
      ; enable sheet selection
      (let [sheets (-> dialog-data props/get :sheets)]
        ; bind selected sheet
        (props/bind (props/entry-property dialog-data, [:sheet-index])
          (props/selected-index-property spreadsheet-combobox))
        ; set sheet names
        (jfx/setup-combobox-first-selected! spreadsheet-combobox, sheets))
      ; remove sheet controls
      (let [{:keys [settings-gridpane, spreadsheet-label, spreadsheet-combobox]} control-map]
        (-> settings-gridpane
          (jfx/remove-children spreadsheet-label, spreadsheet-combobox)
          (jfx/shift-rows -1)
          jfx/clear-row-constraints)))

    ; column names
    (props/bind (props/entry-property dialog-data, [:column-names])
      (props/fn-property
        (fn [{:keys [column-count]}]
          (if (and column-count (pos? column-count))
            (mapv t/excel-letters (range column-count))
            []))
        (props/entry-property dialog-data, [:selected-sheet])))

    ; row names
    (props/bind (props/entry-property dialog-data, [:row-names])
      (props/fn-property
        (fn [{:keys [row-count]}]
          (if (and row-count (pos? row-count))
            (mapv (comp str inc) (range row-count))
            []))
        (props/entry-property dialog-data, [:selected-sheet])))

    ; fill comboboxes with row/column names
    (fill-layout-combobox dialog-data, :genes, gene-names-combobox)
    (fill-layout-combobox dialog-data, :genes, gene-groups-combobox)
    (fill-layout-combobox dialog-data, :samples, sample-ids-combobox)
    (fill-layout-combobox dialog-data, :samples, sample-groups-combobox)))


(defn row-index-cell-factory
  [_]
  (let [label (doto (Label.)
                (.setAlignment Pos/CENTER))]
    (doto (CustomTableCell. label, (props/property label, :text), str)
      (-> .getStyleClass (.add "my-table-header-cell")))))


(defn some=
  [a, b]
  (and a b (= a b)))



(defn cell-type
  [{:keys [layout, layout-type]}, {:keys [row, column]}]
  (let [genes-index (case layout-type
                      :samples-in-columns column
                      :samples-in-rows row)
        samples-index (case layout-type
                        :samples-in-columns row
                        :samples-in-rows column)
        {:keys [gene-names-index, gene-groups-index, sample-ids-index, sample-groups-index]} layout
        gene-name? (some= gene-names-index genes-index)
        gene-group? (some= gene-groups-index genes-index)
        sample-id? (some= sample-ids-index samples-index)
        sample-group? (some= sample-groups-index samples-index)]
    (if (and (or gene-name? gene-group?) (or sample-id? sample-group?))
      :overlap
      (cond
        gene-name? :gene-name
        gene-group? :gene-group
        sample-id? :sample-id
        sample-group? :sample-group))))


(defn cell-position->color
  [dialog-data-map, marker-config, cell-position, value]
  (if-let [type (cell-type dialog-data-map, cell-position)]
    (get cell-colors type)
    (when-let [marker (get marker-config (normalize-marker value))]
      (get cell-colors marker))))


(defn spreadsheet-cell-position-property
  [cell]
  (props/fn-property
    (fn [position]
      (when position
        (update-in position [:column] dec)))
    (table/cell-position-property cell)))

(defn content-cell-factory
  [dialog-data, marker-config-prop _]
  (let [label (doto (Label.)
                (.setAlignment Pos/CENTER))
        cell (CustomTableCell. label, (props/property label, :text), str)
        position-prop (spreadsheet-cell-position-property cell)]
    (props/bind (props/property cell, :style)
      (props/fn-property
        (fn [data-map, marker-config, position, value]
          (when-let [color (cell-position->color data-map, marker-config, position, value)]
            (format "-fx-background-color: %s" color)))
        dialog-data
        marker-config-prop
        position-prop
        (props/property cell, :value)))
    (doto cell
      (-> .getStyleClass (.add "my-table-cell")))))


(def ^:const maximal-cell-count 10000)

(defn create-table-data
  [content-cell-factory, note-label, {:keys [row-count, column-count, rows] :as selected-sheet}]
  (if selected-sheet
    (let [cell-count (* column-count row-count)
          [column-count, row-count, note-message] (if (> cell-count maximal-cell-count)
                                                    (let [quotient (/ (double maximal-cell-count) cell-count)
                                                          shrink-ratio (Math/sqrt quotient)
                                                          visible-column-count (min column-count (max (long (* column-count shrink-ratio)), 10))]
                                                      [visible-column-count, (quot maximal-cell-count visible-column-count),
                                                       (format "Note: Only a part of the dataset is shown (%.3f%%) since the complete dataset is too large."
                                                         (* quotient 100))])
                                                    [column-count, row-count])]
      (props/set-property note-label, :text, note-message)
      {:columns (into [{:attribute :row-index, :width 50, :cell-factory row-index-cell-factory}]
                  (mapv
                    (fn [column-index]
                      {:attribute column-index, :name (t/excel-letters column-index)
                       :min-width Region/USE_COMPUTED_SIZE, :max-width 300
                       :cell-factory content-cell-factory
                       :font {:weight :bold}})
                    (range column-count)))
       :rows (mapv
               (fn [row-index]
                 (let [row (nth rows row-index)]
                   (merge {:row-index (inc row-index)} (zipmap (range column-count) row))))
               (range row-count))})
    {:columns [], :rows []}))




(defn colored-radio-button-cell
  [toggle-group-cache, {:keys [tooltip, color]}, _]
  (let [radiobutton (RadioButton.)
        cell (CustomTableCell. radiobutton, (props/property radiobutton, :selected))]
    ; add radio button to toggle group with the other radio buttons of its row
    (props/listen-to
      (fn [[old-row-index], [new-row-index]]
        (when-not (= old-row-index new-row-index)
          (when-let [^ToggleGroup old-group (some-> toggle-group-cache deref (get old-row-index))]
            (-> old-group .getToggles (.remove radiobutton)))
          (let [^ToggleGroup new-group (-> (swap! toggle-group-cache update new-row-index (fn [group] (or group (ToggleGroup.))))
                                         (get new-row-index))]
            (-> new-group .getToggles (.add radiobutton)))))
      (props/property cell, :index))
    (when color
      (.setStyle radiobutton (format "-fx-base: %s;" (-> color (col/lighter 0.25) col/color->str))))
    (when tooltip
      (jfx/tooltip! radiobutton, tooltip))
    cell))


(defn gather-markers
  [{:keys [rows] :as selected-sheet}, {:keys [layout-type, layout]}]
  (let [{:keys [gene-names-index
                gene-groups-index
                sample-ids-index
                sample-groups-index]} layout
        gene-index-set (cond-> #{gene-names-index}
                         gene-groups-index (conj gene-groups-index))
        sample-index-set (cond-> #{}
                           sample-ids-index (conj sample-ids-index)
                           sample-groups-index (conj sample-groups-index))
        [ignored-rows, ignored-columns] (if (= layout-type :samples-in-columns)
                                          [sample-index-set, gene-index-set]
                                          [gene-index-set, sample-index-set])]
    (persistent!
      (u/reduce-indexed
        (fn [marker-set, row-index, row]
          ; shortcut for ignored rows
          (if (contains? ignored-rows row-index)
            marker-set
            (u/reduce-indexed
              (fn [marker-set, column-index, cell-value]
                (if (contains? ignored-columns column-index)
                  marker-set
                  (conj! marker-set (normalize-marker cell-value))))
              marker-set
              row)))
        (transient #{})
        rows))))


(defn create-marker-rows
  [selected-sheet, layout-map]
  (let [marker-set (gather-markers selected-sheet, layout-map)]
    (->> marker-set
      sort
      (mapv
        (fn [marker]
          {:marker marker, :alteration false, :wild-type false, :missing false})))))


(defn enabled-marker-type
  [marker-state-map]
  (reduce
    (fn [_, marker-type]
      (when (get marker-state-map marker-type)
        (reduced marker-type)))
    nil
    [:alteration, :wild-type, :missing]))


(defn marker-config-property
  [marker-config-table]
  (props/fn-property
    (fn [rows]
      (persistent!
        (reduce
          (fn [result-map, {:keys [marker] :as marker-config}]
            (assoc! result-map marker (enabled-marker-type marker-config)))
          (transient {})
          rows)))
    (table/rows-property marker-config-table)))


(defn determine-layout-config
  [dialog-data]
  (select-keys (props/get dialog-data) [:selected-sheet, :layout-type, :layout]))


(defn check-config
  [layout-config,
   state-marker-data]
  (into
    (reduce
      (fn [errors, kw]
        (let [value (get layout-config kw)]
          (cond-> errors
            (and value (neg? value))
            (conj (if (= kw :gene-names-index)
                    "You have to specify the column that contains the gene names!"
                    (format "When you specify that a %1$s column exist, you have to specify the column that contains the %1$ss!"
                      (case kw
                        :gene-groups-index "gene group",
                        :sample-ids-index "sample id",
                        :sample-groups-index "sample group")))))))
      []
      [:gene-names-index, :gene-groups-index, :sample-ids-index, :sample-groups-index])
    (reduce-kv
      (fn [errors, marker, marker-type]
        (cond-> errors
          (nil? marker-type)
          (conj (format "You have to specify the meaning of \"%s\" (alteration, wild type, missing value)!" (or marker "")))))
      []
      state-marker-data)))


(defn transform-to-alteration-data
  "Transforms alteration entries to 1, 0 or :missing based on the state-marker-data.
  Groups, names and ids are left unmodified."
  [rows, layout-config, marker-config]
  (u/mapv-indexed
    (fn [row-index, row]
      (u/mapv-indexed
        (fn [column-index, value]
          (if (cell-type layout-config, {:row row-index, :column column-index})
            value
            (let [marker-type (get marker-config (normalize-marker value))]
              (case marker-type
                :alteration 1
                :wild-type 0
                :missing :missing))))
        row))
    rows))


(defn import-matrix-data
  [file, layout-config, state-marker-data, alteration-type]
  (let [{:keys [layout-type, layout, selected-sheet]} layout-config
        {:keys [gene-names-index, gene-groups-index, sample-ids-index, sample-groups-index]} layout
        {:keys [column-count, row-count, rows]} selected-sheet
        ; normalization before transposing - since predicates are built from layout-config :data-layout (which would have to be modified here)
        rows (transform-to-alteration-data rows, layout-config, state-marker-data)
        [rows, column-count] (if (= layout-type :samples-in-rows)
                               [(u/transpose rows), row-count]
                               [rows, column-count])]
    (imp/matrix->gene-sample-data (u/file-name file), alteration-type, column-count, gene-names-index, gene-groups-index, sample-ids-index, sample-groups-index, rows)))


(defn show-dialog
  [ui, file, completion-fn]
  (let [file-type (u/import-file-type file)]
    (if-let [data (case file-type
                    :csv (imp/import-csv-file file, true),
                    :excel (imp/import-excel-file file))]
      (jfx/run-now
        (let [dialog-data-atom (atom {:sheets (when (= file-type :excel)
                                                (mapv :sheet-name (:sheets data)))})
              dialog-data (props/data-property dialog-data-atom)
              control (jfx/create-control "avatar/ui/ImportDialog.fxml")
              control-node (jfx/control-node control)
              {:keys [data-table
                      marker-config-table
                      note-label
                      import-button
                      cancel-button]
               :as control-map} (jfx/control-children control)
              window (doto (jfx/modal-window
                             (format "Import data from %s file" (case file-type :csv "CSV" :excel "Excel")),
                             control-node,
                             1024,
                             512)
                       (t/add-avatar-icon))
              selected-sheet-prop (props/entry-property dialog-data, [:selected-sheet])
              toogle-group-cache (atom {})]

          #_(t/watch-property dialog-data)



          (setup-comboboxes dialog-data, file-type, control-map)

          (table/setup-table data-table, [], {})

          ; setup marker config table
          (table/setup-table marker-config-table,
            [{:name "Value"
              :attribute :marker
              :sortable? true
              :font {:weight :bold}}
             {:name "Alt."
              :attribute :alteration
              :cell-factory (partial colored-radio-button-cell toogle-group-cache, {:color (:alteration cell-colors), :tooltip "Mark as alteration encoding."})
              :editable? true
              :sortable? true
              :font {:weight :bold}
              :width 50}
             {:name "WT"
              :attribute :wild-type
              :cell-factory (partial colored-radio-button-cell toogle-group-cache, {:color (:wild-type cell-colors), :tooltip "Mark as wild type encoding."})
              :editable? true
              :sortable? true
              :font {:weight :bold}
              :width 50}
             {:name "miss."
              :attribute :missing
              :cell-factory (partial colored-radio-button-cell toogle-group-cache, {:color (:missing cell-colors), :tooltip "Mark as missing value encoding."})
              :editable? true
              :sortable? true
              :font {:weight :bold}
              :width 50}]
            {:sort? true})
          ; bind rows of marker config table based on sheet data and specified layout
          (props/bind (table/rows-property marker-config-table)
            (props/fn-property
              create-marker-rows
              selected-sheet-prop
              (props/selected-keys-property dialog-data, #{:layout-type, :layout})))

          ; before binding selected sheet to ensure calling this listener
          (props/listen-to
            (fn [_, [selected-sheet]]
              (props/set (table/data-property data-table)
                (create-table-data
                  (partial content-cell-factory dialog-data, (marker-config-property marker-config-table))
                  note-label
                  selected-sheet)))
            selected-sheet-prop)

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


          (jfx/handle-event! import-button, :action,
            (fn [_]
              (let [layout-config (determine-layout-config dialog-data)
                    marker-config (props/get (marker-config-property marker-config-table))]
                (if-let [error-messages (seq (check-config layout-config, marker-config))]
                  (jfx/show-error "Import Error", "Incomplete import configuration",
                    (str/join "\n" error-messages))
                  (let [alteration-type (t/ask-for-alteration-type #{:mutation-data, :expression-data, :methylation-data},
                                          "Specify type of alteration data"
                                          "Which kind of alteration data are you importing?")
                        gene-sample-data (import-matrix-data file, layout-config, marker-config, alteration-type)]
                    (jfx/close window)
                    (u/safe-future
                      (completion-fn gene-sample-data)))))))

          (jfx/handle-event! cancel-button, :action,
            (fn [_]
              (jfx/close window)))

          (jfx/show window)))
      ; reading input file failed
      (jfx/show-error "Import Error",
        "Could not import alteration matrix."
        (format "The %s file \"%s\" does not contain a valid alteration matrix!"
          (name file-type), file)))))



(comment
  (import-matrix-dialog/show-dialog nil, (io/file "/home/gv/workspace/avatar/datasets/JCO_Verena_Datensatz_NPM1_FLT3_biCEBPA_VG_29082016_selected groups_test2.xls")
    (fn [& args]
      (println "import matrix success")
      (prn args)))
  )