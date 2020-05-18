; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.import
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.data.int-map :as im]
    [avatar.util :as u]
    [avatar.data.csv :as csv]
    [avatar.algorithms.common :as c]
    [avatar.ui.tools :as t]
    [avatar.ui.dialogs.progress-view :as progress])
  (:import
    (org.apache.poi.hssf.usermodel HSSFWorkbook)
    (org.apache.poi.xssf.usermodel XSSFWorkbook)
    (org.apache.poi.ss.usermodel Workbook Sheet Row Cell)
    (java.io Closeable)
    (com.monitorjbl.xlsx StreamingReader)))



(defn workbook
  ^Workbook [file]
  (let [ext (str/lower-case (u/file-extension file))]
    (case ext
      "xls" (HSSFWorkbook. (io/input-stream file))
      "xlsx" (-> (StreamingReader/builder)
               (.rowCacheSize 10)
               (.bufferSize 4096)
               (.open (io/input-stream file))))))


(defn sheets
  [^Workbook workbook]
  (iterator-seq (.sheetIterator workbook)))


(defn sheet-name
  [^Sheet sheet]
  (.getSheetName sheet))


(defn rows
  [^Sheet sheet]
  (iterator-seq (.rowIterator sheet)))


(defn first-cell-index
  [^Row row]
  (.getFirstCellNum row))


(defn last-cell-index
  [^Row row]
  (unchecked-dec (.getLastCellNum row)))


(defn cells
  [^Row row]
  (iterator-seq (.cellIterator row)))


(defn cell-at
  ^Cell [^Row row, ^long column-index]
  (.getCell row, column-index))


(defn column-index
  ^long [^Cell cell]
  (.getColumnIndex cell))


(defn row-index
  ^long [^Cell cell]
  (.getRowIndex cell))


(defn cell-value
  [^Cell cell]
  (u/case+ (.getCellType cell)
    Cell/CELL_TYPE_STRING (.getStringCellValue cell)
    Cell/CELL_TYPE_BOOLEAN (.getBooleanCellValue cell)
    Cell/CELL_TYPE_NUMERIC (.getNumericCellValue cell)
    nil))


(defn row-sizes
  [^Sheet sheet]
  (mapv
    (comp count cells)
    (rows sheet)))


(defn almost-integer?
  [^double value]
  (< (Math/abs (- value (Math/round value))) 1.0E-9))


(defn value->str
  [value]
  (cond
    (string? value) (when-not (str/blank? value) value)
    ; only true floats (not almost integers) are converted to floating point string representations
    (float? value) (str (cond-> (double value) (almost-integer? value) (-> Math/round long)))
    (integer? value) (str value)
    :else value))


(defn empty-columns
  [^Sheet sheet, column-count]
  (let [all-columns (c/populated-dense-int-set column-count),
        empty-cells-per-row (mapv
                              (fn [^Row row]
                                (u/reduce-indexed
                                  (fn [empty-cells, cell-index, cell]
                                    (cond-> empty-cells
                                      (cell-value cell)
                                      (disj cell-index)))
                                  all-columns
                                  (cells row)))
                              (rows sheet))]
    (reduce im/intersection (c/populated-dense-int-set column-count) empty-cells-per-row)))


(defn empty-row?
  [row]
  (every?
    (fn empty-cell? [cell]
      (let [value (cell-value cell)]
        (or
          (nil? value)
          (and (string? value) (str/blank? value)))))
    row))


(defn empty-rows
  [^Sheet sheet]
  (u/reduce-indexed
    (fn [empty-rows, row-index, row]
      (cond-> empty-rows
        (empty-row? row)
        (conj row-index)))
    (im/dense-int-set)
    (rows sheet)))


(defn sheet-info
  [^Sheet sheet]
  (let [row-count (count (rows sheet)),
        column-count (reduce
                       (fn [max-column-index, row]
                         (max max-column-index, (last-cell-index row)))
                       0
                       (rows sheet))
        empty-columns (empty-columns sheet, column-count)
        empty-rows (empty-rows sheet)]
    {:row-count (- row-count (count empty-rows)),
     :column-count (- column-count (count empty-columns)),
     :empty-columns empty-columns,
     :empty-rows empty-rows}))


(defn process-row-old
  "Extract values from the cells of the given row into a vector of length given by the column count."
  [{:keys [empty-columns, column-count]}, ^Row row]
  (persistent!
    (reduce
      (fn [cells-vec, column-index]
        (if (contains? empty-columns column-index)
          cells-vec
          (let [value (some-> (cell-at row, column-index)
                        cell-value
                        value->str)]
            (conj! cells-vec value)))
        )
      (transient [])
      (range column-count))))


(defn process-row
  [^Row row]
  (let [cell-it (.cellIterator row)]
    (loop [column->value-map (transient {})]
      (if (.hasNext cell-it)
        (let [cell (.next cell-it)
              column-index (column-index cell)
              value (some-> cell cell-value value->str)]
          (recur
            (cond-> column->value-map
              (some? value)
              (assoc! column-index value))))
        (let [column->value-map (persistent! column->value-map)
              non-empty-cells (into (im/dense-int-set) (keys column->value-map))
              column-count (if (seq non-empty-cells)
                             (reduce max non-empty-cells)
                             0)]
          (when (seq non-empty-cells)
            {:row-values (mapv #(get column->value-map %) (range (inc column-count)))
             :column-count column-count
             :non-empty-cells non-empty-cells}))))))


(defn remove-empty-columns
  [non-empty-columns, row]
  (let [n (count row)]
    (persistent!
      (reduce
        (fn [new-row, old-index]
          (conj! new-row
            ; when old-index exceeds row size append nil
            (when (< old-index n)
              (nth row old-index))))
        (transient [])
        non-empty-columns))))


(defn process-sheet
  [^Sheet sheet]
  (let [sheet-name (sheet-name sheet)
        row-it (.rowIterator sheet)]
    (loop [rows (transient []), non-empty-columns (im/dense-int-set)]
      (if (.hasNext row-it)
        (let [row (.next row-it)]
          (if-let [{:keys [row-values, column-count, non-empty-cells]} (process-row row)]
            (recur
              (conj! rows row-values)
              (im/union non-empty-columns non-empty-cells))
            (recur
              rows
              non-empty-columns)))
        ; done: create sheet map
        (let [rows (persistent! rows)
              row-count (count rows)
              column-count (count non-empty-columns)]
          (when (pos? row-count)
            {:sheet-name sheet-name
             :row-count row-count
             :column-count column-count
             :rows (mapv (partial remove-empty-columns non-empty-columns) rows)}))))))


(defn import-excel-file
  [file]
  (progress/with-shown-indeterminate-progress (format "Reading Excel file \"%s\"" (u/file-name file))
    (with-open [wb (workbook file)]
      (let [sheets (->> wb
                     sheets
                     (u/keepv process-sheet))]
        (when (seq sheets)
          (hash-map :type :excel, :file (u/file-absolute-path file), :sheets sheets))))))



(defn check-input
  [matrix?, data]
  (cond
    (== (count data) 0)
    :error/no-row
    (== (count data) 1)
    :error/only-one-row
    matrix? (let [row-sizes (->> data (mapv count) distinct)]
              (if (some #(< % 2) row-sizes)
                :error/only-one-column
                data))
    :else (let [row-sizes (->> data (mapv count) distinct)]
            ; table should have more than one column
            (if (some #(> % 1) row-sizes)
              data
              :error/only-one-column))))


(defn guess-separator
  [file]
  (let [comma (int \,),
        semicolon (int \;),
        tab (int \tab)]
    (with-open [rdr (io/reader file)]
      (loop [comma-count 0, semicolon-count 0, tab-count 0]
        (let [x (.read rdr)]
          (if (>= x 0)
            (recur
              (cond-> comma-count
                (== comma x) unchecked-inc),
              (cond-> semicolon-count
                (== semicolon x) unchecked-inc)
              (cond-> tab-count
                (== tab x) unchecked-inc))
            (->> [[\, comma-count], [\; semicolon-count], [\tab tab-count]]
              (remove (comp zero? second))
              (sort-by second >))))))))


(defn read-csv-file
  "Read the given file and check whether the read data is a valid data matrix.
  Otherwise, a keyword identifying the error reason is returned.
  When no separator is given, the function tries the separators (\\, \\; \\tab) found in the file in the order of their occurence count until no error occurs.
  In case no separator produces a valid data matrix, a map of separator to error reason is returned."
  ([file, matrix?]
    ; guessing separator from comma, semicolon and tab
   (reduce
     (fn [error-map, sep]
       (let [data (read-csv-file file, matrix?, sep)]
         (if (keyword? data)
           (assoc error-map sep data)
           (reduced data))))
     {}
     (mapv first (guess-separator file))))
  ([file, matrix?, separator]
   (with-open [rdr (io/reader file)]
     (->> (csv/read-csv rdr, :separator separator)
       vec
       (check-input matrix?)))))


(defn process-csv-row
  [column-count, row]
  (let [n (count row)
        row (mapv value->str row)]
    (if (< n column-count)
      (into row (repeat (- column-count n) ""))
      row)))


(defn import-csv-file
  [file, matrix?]
  (let [rows (read-csv-file file, matrix?)]
    (when-not (map? rows)
      (let [column-count (reduce max (mapv count rows))]
        (->> rows
          (mapv (partial process-csv-row column-count))
          (hash-map :type :csv, :file (u/file-absolute-path file), :column-count column-count, :row-count (count rows), :rows))))))


(defn row->int-set
  [set-bit?, alterations]
  (let [alterations (cond-> alterations (not (vector? alterations)) vec),
        n (count alterations)]
    (loop [sample 0, s (im/dense-int-set)]
      (if (< sample n)
        (let [v (nth alterations sample)]
          (recur
            (inc sample),
            (cond-> s (set-bit? v) (conj sample))))
        s))))


(def group+gene (juxt :group :gene))


(defn merge-alterations
  [gene-1, gene-2]
  (let [alteration-types (reduce
                           (fn [type-set, gene]
                             (into type-set (-> gene :alteration-data keys)))
                           [gene-1, gene-2])]
    (reduce
      (fn [result-gene, atype]
        (let [alteration-map-1 (get-in gene-1 [:alteration-data, atype])
              alteration-map-2 (get-in gene-2 [:alteration-data, atype])]
          (assoc-in result-gene [:alteration-data, atype]
            {:alterations
             (into (or (:alterations alteration-map-1) (im/dense-int-set))
               (:alterations alteration-map-2)),
             :missing-values
             (into (or (:missing-values alteration-map-1) (im/dense-int-set))
               (:missing-values alteration-map-2))})))
      gene-1
      alteration-types)))


(defn merge-duplicates
  [gene-list]
  (let [order (->> gene-list (map group+gene) distinct vec)
        merged-genes-map (persistent!
                           (reduce
                             (fn [merged-genes-map, gene]
                               (u/update-in! merged-genes-map [(group+gene gene)]
                                 (fn [prev-gene]
                                   (if prev-gene
                                     (merge-alterations prev-gene, gene)
                                     gene))))
                             (transient {})
                             gene-list))]
    (mapv
      #(get merged-genes-map %)
      order)))


(defn matrix->gene-sample-data
  "Converts a data matrix (e.g. read from a csv file) to sample alteration data."
  [study-alias, alteration-type, column-count, gene-names-index, gene-groups-index, sample-ids-index, sample-groups-index, rows]
  (assert (t/alteration-type? alteration-type) "must be an alteration type")
  (let [sample-count (cond-> column-count
                       gene-names-index dec
                       gene-groups-index dec)
        ; function to extract alteration data from rows (or sample group names)
        extract-sample-data (if (or gene-names-index gene-groups-index)
                              (let [columns-to-delete (cond-> #{}
                                                        gene-names-index (conj gene-names-index)
                                                        gene-groups-index (conj gene-groups-index))]
                                (fn [row]
                                  (u/delete columns-to-delete, row)))
                              identity)
        gene-list (mapv
                    (fn row->gene-sample-data [i, row]
                      (let [sample-data (extract-sample-data row)]
                        (-> {:gene (some-> (nth row gene-names-index) str/trim)
                             :order-fixed? false,
                             :group (when gene-groups-index
                                      (let [group (nth row gene-groups-index)]
                                        (u/trim-nil group)))}
                          (assoc-in [:alteration-data, alteration-type]
                            {:alterations (row->int-set #(= % 1) sample-data),
                             :missing-values (row->int-set #(= % :missing) sample-data)}))))
                    (range)
                    (if (or sample-ids-index sample-groups-index)
                      (u/delete
                        (cond-> #{}
                          sample-ids-index (conj sample-ids-index)
                          sample-groups-index (conj sample-groups-index))
                        rows)
                      rows))
        sample-group-map (if sample-groups-index
                           (let [sample-groups (extract-sample-data (nth rows sample-groups-index))]
                             (zipmap (range) (mapv u/trim-nil sample-groups)))
                           (zipmap (range sample-count) (repeat study-alias)))]
    {:gene-list (merge-duplicates gene-list),
     :alteration-type-set #{alteration-type},
     :clinical-attributes (cond-> #{"STUDY"}
                            sample-group-map
                            (conj "SAMPLE_GROUP")),
     :clinical-data-map (persistent!
                          (reduce-kv
                            (fn [clinical-data-map, sample-index, sample-group]
                              (assoc! clinical-data-map, sample-index
                                (cond-> {"STUDY" study-alias}
                                  sample-group-map
                                  (assoc "SAMPLE_GROUP" sample-group))))
                            (transient {})
                            sample-group-map)),
     :sample-count sample-count,
     :sample-id-vec (if sample-ids-index
                      (let [sample-ids (extract-sample-data (nth rows sample-ids-index))]
                        (mapv u/trim-nil sample-ids))
                      ; default sample ids
                      (mapv (comp str inc) (range sample-count))),
     :sample-group-map sample-group-map,
     :sample-permutation {:sample->column (vec (range sample-count)),
                          :column->sample (vec (range sample-count))}}))


(defn attribute-set
  [attribute, alteration-list]
  (persistent!
    (reduce
      (fn [result-set, alteration-data]
        (conj! result-set (get alteration-data attribute)))
      (transient #{})
      alteration-list)))



(defn gene-patient-map->gene-sample-data
  [alteration-type, study-name, sample-id-vec, gene->mutated-samples-map]
  (let [gene-set (set (keys gene->mutated-samples-map)),
        sorted-gene-vec (->> gene-set set sort vec),
        sample-count (count sample-id-vec),
        sample-group-map (zipmap (range sample-count) (repeat study-name)),
        gene-list (mapv
                    (fn [gene-name]
                      (-> {:gene gene-name,
                           :group nil,
                           :order-fixed? false}
                        (assoc-in [:alteration-data, alteration-type]
                          {:alterations (get gene->mutated-samples-map gene-name),
                           ; no information about missing values because of alteration list
                           :missing-values (im/dense-int-set)})))
                    sorted-gene-vec)]
    {:gene-list gene-list,
     :alteration-type-set #{alteration-type},
     :clinical-attributes #{"STUDY"},
     :clinical-data-map (zipmap (range sample-count) (repeat {"STUDY" study-name})),
     :sample-count sample-count,
     :sample-id-vec sample-id-vec,
     :sample-group-map sample-group-map,
     :sample-permutation {:sample->column (vec (range sample-count)),
                          :column->sample (vec (range sample-count))}}))


(defn alteration-list->gene-sample-data
  [alteration-type, study-name, alteration-list]
  (let [samples (attribute-set :sample, alteration-list),
        sample-id-vec (mapv u/trim-nil samples),
        sample->index (zipmap sample-id-vec (range)),
        gene->mutated-samples-map (persistent!
                                    (reduce
                                      (fn [result-map, {:keys [sample, gene]}]
                                        (u/update-in! result-map [gene]
                                          (fnil conj (im/dense-int-set))
                                          (sample->index sample)))
                                      (transient {})
                                      alteration-list))]
    (gene-patient-map->gene-sample-data alteration-type, study-name, sample-id-vec, gene->mutated-samples-map)))


(defn merge-clinical-data
  [sample-id-vec, clinical-samples-map, clinical-patients-map]
  (if (seq clinical-patients-map)
    (let [sample-id-set (set sample-id-vec)]
      (persistent!
        (reduce-kv
          (fn [result-map, sample-id, sample-data-map]
            (let [patient-id (get sample-data-map "PATIENT_ID")
                  patient-data-map (get clinical-patients-map patient-id)]
              (cond-> result-map
                (contains? sample-id-set sample-id)
                (assoc!
                  sample-id
                  ; merge patient data onto sample data
                  (merge sample-data-map patient-data-map)))))
          (transient {})
          clinical-samples-map)))
    ; no additional patient data
    (persistent!
      (reduce-kv
        (fn [result-map, sample-id, sample-data-map]
          (assoc! result-map sample-id sample-data-map))
        (transient {})
        clinical-samples-map))))


(defn rename-sample-ids->index
  [sample-id->index, clinical-data-map]
  (persistent!
    (reduce-kv
      (fn [result-map, clinical-sample-id, clinical-sample-data]
        (if-let [index (get sample-id->index clinical-sample-id)]
          (assoc! result-map index clinical-sample-data)
          ; we have clinical data for a sample which is not present in the alteration data or expression data
          (do
            (u/log-debug "No index found for sample with id \"%s\"!" clinical-sample-id)
            result-map)))
      (transient {})
      clinical-data-map)))


(defn sample-id-renaming
  "Returns a map that contains renaming rules for sample ids with additional suffix that contain the clinical sample id as prefix."
  [clinical-samples, sample-id-set]
  (let [clinical-sample-ids (->> clinical-samples keys (map str/trim) set)]
    (if (or (empty? sample-id-set) (= sample-id-set clinical-sample-ids))
      ; identity mapping
      (zipmap clinical-sample-ids clinical-sample-ids)
      ; create mapping
      (persistent!
        (reduce
          (fn [renaming-map, sample-id]
            (if (contains? clinical-sample-ids sample-id)
              (assoc! renaming-map sample-id sample-id)
              (let [matching-sample-ids (filterv #(.startsWith ^String sample-id ^String %) clinical-sample-ids),
                    n (count matching-sample-ids)]
                (cond
                  ; exactly one match, add renaming rule
                  (== n 1) (assoc! renaming-map sample-id (first matching-sample-ids))
                  ; no match, keep the sample-id unchanged
                  (== n 0) (assoc! renaming-map sample-id sample-id)
                  :else (u/runtime-exception "Multiple clinical sample ids found for sample id \"%s\" from alteration data." sample-id)))))
          (transient {})
          sample-id-set)))))


(defn add-study-name
  [study-name, clinical-data-map]
  (if (str/blank? study-name)
    clinical-data-map
    (persistent!
      (reduce-kv
        (fn [result-map, sample-id, sample-data]
          (assoc! result-map sample-id (assoc sample-data "STUDY" study-name)))
        (transient {})
        clinical-data-map))))


(defn clinical-attributes
  [clinical-data-map]
  (->> clinical-data-map
    (reduce-kv
      (fn [results-set, _, sample-data-map]
        (u/into! results-set (keys sample-data-map)))
      (transient #{}))
    persistent!))


(defn add-clinical-data
  [study-name, {:keys [clinical-samples, clinical-patients] :as study-data-map}, {:keys [sample-id-vec] :as gene-sample-data}]
  (let [sample-id->index (zipmap sample-id-vec (range))]
    (if (and (seq sample-id-vec) (seq clinical-samples))
      (let [clinical-data-map (add-study-name study-name,
                                (rename-sample-ids->index sample-id->index,
                                  (merge-clinical-data sample-id-vec, clinical-samples, clinical-patients)))]
        (assoc gene-sample-data
          :clinical-attributes (clinical-attributes clinical-data-map),
          :clinical-data-map clinical-data-map))
      ; no clinical data or no sample/patient ids
      gene-sample-data)))


(defn alteration-list->gene-alterations-per-sample
  [sample-id->index-map, alteration-data-list]
  (->> alteration-data-list
    (reduce
      (fn [result-map, {:keys [sample, gene]}]
        (u/update-in! result-map [gene, :alterations]
          (fnil conj (im/dense-int-set))
          (get sample-id->index-map sample)))
      (transient {}))
    persistent!
    ; add empty :missing-values sets
    (reduce-kv
      (fn [result-map, gene, gene-data]
        (assoc! result-map gene (assoc gene-data :missing-values (im/dense-int-set))))
      (transient {}))
    persistent!))


(defn value-array->alteration-set
  [altered?-fn, sample-id->index-map, sample-ids, ^doubles value-array]
  (let [n (count sample-id->index-map)
        sample-id->array-index (zipmap sample-ids (range))]
    (reduce-kv
      (fn [gene-data, sample-id, sample-index]
        (if-let [array-index (get sample-id->array-index sample-id)]
          ; if altered, add to alterations
          (let [value (aget value-array array-index)]
            (cond-> gene-data

              (and (not (Double/isNaN value)) (altered?-fn value))
              (update-in [:alterations] conj sample-index)

              (Double/isNaN value)
              ; add to missing values
              (update-in [:missing-values] conj sample-index)))
          ; add to missing values
          (update-in gene-data [:missing-values] conj sample-index)))
      {:alterations (im/dense-int-set),
       :missing-values (im/dense-int-set)}
      sample-id->index-map)))


(defn alteration-matrix-gene-alterations-per-sample
  [altered?-fn, sample-id->index-map, {:keys [values-per-gene, sample-ids], :as alteration-matrix-data}]
  (persistent!
    (reduce
      (fn [result-map, {:keys [hugo-symbol, entry-gene-id, values]}]
        (assoc! result-map
          (or hugo-symbol entry-gene-id)
          (value-array->alteration-set altered?-fn, sample-id->index-map, sample-ids, values)))
      (transient {})
      values-per-gene)))
