; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.geo
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [frost.quick-freeze :as qf]
    [avatar.data.csv :as csv]
    [avatar.util :as u]
    [avatar.data.bioportal :as bp]
    [clj-jfx.core :as jfx]
    [avatar.ui.dialogs.progress-view :as progress]))


(defn mapping-meta-row?
  [row]
  (boolean (some->> row first first (contains? #{\# \! \^}))))


(defn find-column-positions
  [column->fn-map, column-row]
  (persistent!
    (reduce-kv
      (fn [position-map, column, column-fn]
        (u/reduce-indexed
          (fn [position-map, index, data-column]
            (let [data-column (u/normalize-str data-column)]
              (if (column-fn data-column)
                ; found the column -> done
                (reduced (assoc! position-map column index))
                ; continue
                position-map)))
          position-map
          column-row))
      (transient {})
      column->fn-map)))


(defn gene-symbol-mapping-data
  [readable]
  (transduce
    (remove mapping-meta-row?)
    (fn
      ([rows]
       (persistent! rows))
      ([rows, r]
       (conj! rows r)))
    (transient [])
    (csv/csv-rows-reducible readable, "\t", "\"", "#", true)))


(defn extract-attributes-from-row
  [position-map, row]
  (let [n (count row)]
    (persistent!
      (reduce-kv
        (fn [result-map, column, position]
          (assoc! result-map column (when (< position n) (nth row position))))
        (transient {})
        position-map))))


(defn create-mapping
  [columns, rows, selected-column]
  (let [selected-column (u/normalize-str selected-column),
        position-map (find-column-positions
                       {:id #(= % "id"),
                        :gene-symbol #(= % selected-column)},
                       columns)]
    (transduce
      (comp
        (map (partial extract-attributes-from-row position-map))
        (filter :id))
      (completing
        (fn [result-map, {:keys [id, gene-symbol]}]
          (cond-> result-map
            (not (str/blank? gene-symbol))
            (assoc! id gene-symbol)))
        persistent!)
      (transient {})
      rows)))


(defn series-info-url
  [gse-id]
  (format "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s" gse-id))


(defn series-filename
  [series-id]
  (str series-id "_series_matrix.txt.gz"))

(defn series-folder-url
  [series-id]
  (let [group-folder (->> series-id (re-matches #"(GSE\d+)\d{3,3}") second)]
    (format "ftp://ftp.ncbi.nlm.nih.gov/geo/series/%snnn/%s/matrix/"
      group-folder, series-id)))

(defn download-series-matrix
  [directory, series-id]
  (let [url (str (series-folder-url series-id) (series-filename series-id)),
        destination-file (io/file directory (series-filename series-id))]
    (when-not (and
                (u/file-exists? destination-file)
                (jfx/ask-confirmation "Import Data from Gene Expression Omnibus",
                  "Do you want to use the previously downloaded series matrix?",
                  (format "The file has been downloaded previously to the following location:\n%s"
                    (str destination-file))))
      (bp/ensure-directory directory)
      (progress/with-shown-indeterminate-progress (format "Downloading expression data for %s ..." series-id)
        (u/download-file url, destination-file)))
    destination-file))


(defn meta-row?
  [row]
  (boolean (some-> row first (str/starts-with? "!"))))


(defn clinical-data-map
  [study-id, clinical-data-rows]
  (let [attributes (->> clinical-data-rows first rest)]
    {:clinical-attributes (conj (set attributes) "STUDY")
     :clinical-data-map (persistent!
                          (reduce
                            (fn [result-map, [sample-id, & attribute-values]]
                              (assoc! result-map sample-id
                                (-> (zipmap attributes attribute-values)
                                  (assoc "STUDY" study-id))))
                            (transient {})
                            (rest clinical-data-rows)))}))


(defn parse-clinical-attributes
  [study-id, sample-ids, meta-rows]
  (->> meta-rows
    (keep
      (fn [row]
        (when-let [attribute (some->> row first (re-matches #"!Sample_(.*)") second)]
          (if (re-matches #"characteristics_ch.*" attribute)
            (let [tag-value-pairs (mapv
                                    (fn [cell]
                                      (str/split cell #": "))
                                    (rest row))
                  attribute (->> tag-value-pairs first first)]
              (list* attribute (mapv second tag-value-pairs)))
            (list* attribute (rest row))))))
    (list* (list* "SAMPLE_ID" sample-ids))
    (u/transpose)
    (clinical-data-map study-id)))


(defn find-series-attribute
  [attribute, meta-rows]
  (reduce
    (fn [_, [label, value]]
      (when (= (str/lower-case (str/trim label)) attribute)
        (reduced value)))
    nil
    meta-rows))


(defn find-series-attribute-multiples
  [attribute, meta-rows]
  (reduce
    (fn [results, [label, value]]
      (cond-> results
        (= (str/lower-case (str/trim label)) attribute)
        (conj value)))
    []
    meta-rows))


(defn author-name
  [name-str]
  (let [name-parts (str/split name-str #",")]
    (str (last name-parts) " " (str/join (keep first (butlast name-parts))))))


(defn import-gse-file
  "Extracts the clinical and alteration data from a GSE file.
  https://www.ncbi.nlm.nih.gov/geo/info/soft.html"
  [gse-readable]
  (let [rows (csv/read-csv gse-readable, :separator "\t"),
        meta-rows (vec (take-while meta-row? rows)),
        data-rows (vec (remove meta-row? rows)),
        platform-ids (find-series-attribute-multiples "!series_platform_id", meta-rows),
        study (find-series-attribute "!series_title", meta-rows),
        summary (find-series-attribute "!series_summary", meta-rows),
        id (find-series-attribute "!series_geo_accession", meta-rows),
        authors (->> (find-series-attribute-multiples "!series_contributor", meta-rows)
                  (map author-name)
                  (str/join ", "))
        header-row (first data-rows),
        _ (assert (-> header-row first (= "ID_REF")) "first data row must be the header with the column names and the first column must be ID_REF")
        sample-ids (rest header-row)]
    (if-let [expression-data (bp/build-value-table :id, "id_ref", #{}, data-rows)]
      (merge
        {:study study,
         :id id,
         :summary summary,
         :authors authors,
         :platform-ids platform-ids,
         :expression-data expression-data}
        (parse-clinical-attributes id, sample-ids, meta-rows))
      (do
        (u/log-debug "No expression data in series matrix for study %s (%s)." id study)
        nil))))


(defn rename-ids->gene-symbols
  [id->gene-mapping, expression-data]
  (update-in expression-data [:values-per-gene]
    (fn [values-per-gene]
      (mapv
        (fn [{:keys [id] :as gene-data}]
          (-> gene-data
            (dissoc :id)
            ; lookup HUGO symbol, default to id
            (assoc :hugo-symbol (get id->gene-mapping id id))))
        values-per-gene))))


(defn id->gene-symbols
  [id->gene-mapping, {:keys [expression-data] :as data}]
  (assoc data
    :expression-data
    (rename-ids->gene-symbols id->gene-mapping, expression-data)))