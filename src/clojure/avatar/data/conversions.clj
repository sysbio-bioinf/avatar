; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.conversions
  (:require
    [avatar.data.import :as imp]
    [avatar.util :as u]
    [clojure.string :as str]
    [clojure.java.io :as io]
    [avatar.data.csv :as csv]))



(defn check-table-format
  [table-data]
  (let [column-count-freqs (->> table-data (mapv count) frequencies)]
    (if (== (count column-count-freqs) 1)
      table-data
      (u/illegal-argument "Not all rows have the same number of columns. The following number of columns have been found: %s"
        (->> column-count-freqs
          (into (sorted-map))
          (map (fn [[column-count freq]] (format "%d x %d cols" freq column-count)))
          (str/join ", "))))))


(defn find-index
  [value, value-vector]
  (let [index (reduce
                (fn [index, v]
                  (if (= v value)
                    (reduced index)
                    (inc index)))
                0
                value-vector)]
    (when (< index (count value-vector))
      index)))


(defn normalize-names
  [column-name]
  (str/trim (str/replace column-name #"\p{Cntrl}" " ")))


(defn extract-gene-sample-data
  [{:keys [additional-columns-map, gene-renaming] :as options}, gene-column, sample-column, table-data]
  (let [table-column-names (->> table-data first (mapv normalize-names)),
        gene-index (find-index gene-column, table-column-names),
        sample-index (find-index sample-column, table-column-names)
        additional-columns-index-map (reduce-kv
                                       (fn [m, column, column-name]
                                         (let [index (find-index column-name, table-column-names)]
                                           (cond-> m index (assoc column index))))
                                       {}
                                       additional-columns-map)]
    ; error handling
    (when-not gene-index
      (u/illegal-argument "Index of gene column \"%s\" could not be found among the column names: %s"
        gene-column, (pr-str table-column-names)))
    (when-not sample-index
      (u/illegal-argument "Index of sample column \"%s\" could not be found among the column names: %s"
        sample-column, (pr-str table-column-names)))
    ; extract data
    (->> table-data
      rest
      (mapv (fn [row]
              (let [mutation-map (reduce-kv
                                   (fn [mutation-map, column, index]
                                     (assoc mutation-map column (nth row index)))
                                   {:gene (normalize-names (nth row gene-index)),
                                    :sample (normalize-names (nth row sample-index))},
                                   additional-columns-index-map)]
                (cond-> mutation-map
                  gene-renaming (update-in [:gene] gene-renaming mutation-map))))))))


(defn check-one-mutation-per-sample-and-gene
  [samples-with-mutations-per-gene]
  (let [genes-with-multiple-mutations (reduce-kv
                                        (fn [result-map, gene, samples]
                                          (let [sample-freqs (->> samples
                                                               frequencies
                                                               (map second)
                                                               (filter #(> % 1))
                                                               frequencies
                                                               (into (sorted-map)))]
                                            (cond-> result-map
                                              (seq sample-freqs) (assoc gene sample-freqs))))
                                        (sorted-map)
                                        samples-with-mutations-per-gene)]
    (when (seq genes-with-multiple-mutations)
      (u/log-error
        (format "%d genes are mutated multiple times per sample:\n%s"
          (count genes-with-multiple-mutations)
          (->> genes-with-multiple-mutations
            (mapv
              (fn [[gene, freqs]]
                (str "  " gene ": " (str/join ", " (mapv #(format "%d (%d)" (first %) (second %)) freqs)))))
            (str/join "\n")))))))


(defn gene+sample-list->table
  [gene+sample-list]
  (let [sample-list (->> gene+sample-list (map :sample) distinct sort vec)
        samples-with-mutations-per-gene (group-by :gene gene+sample-list)]
    (check-one-mutation-per-sample-and-gene samples-with-mutations-per-gene)
    (->> samples-with-mutations-per-gene
      (reduce-kv
        (fn [rows, gene, samples-with-mutation]
          (let [samples-with-mutation-set (->> samples-with-mutation (map :sample) set)]
            (conj rows
              (into [gene]
                (map #(if (contains? samples-with-mutation-set %) "M" "WT") sample-list)))))
        [])
      (sort-by first)
      (list* (list* "sample" sample-list)))))


(defn csv-supplement-table->gene-sample-table
  ([import-filename, gene-column, sample-column, export-filename]
   (csv-supplement-table->gene-sample-table import-filename, gene-column, sample-column, export-filename, {}))
  ([import-filename, gene-column, sample-column, export-filename, {:keys [additional-columns-map, row-filter, gene-renaming, custom-read, list-post-processing] :as options}]
   (let [table-row-maps (->> (if custom-read
                               (custom-read import-filename)
                               (imp/read-csv-file import-filename))
                          check-table-format
                          (extract-gene-sample-data options, gene-column, sample-column))
         table-row-maps (cond->> table-row-maps
                          row-filter (filterv row-filter)
                          list-post-processing list-post-processing)
         table-rows (gene+sample-list->table table-row-maps)]
     (println
       "#genes =" (->> table-row-maps (map :gene) distinct count)
       "#samples =" (->> table-row-maps (map :sample) distinct count)
       "#alterations =" (->> table-row-maps (group-by :gene) vals (map (comp count distinct)) (reduce +)))
     (with-open [w (io/writer export-filename)]
       (csv/write-csv w, table-rows, :separator \;)))))



(defn gene-name?
  [{:keys [gene]}]
  (and
    (re-matches #"[A-Z\d]+" gene)
    (not (re-matches #"LOC\d+" gene))
    (not (re-matches #"ENSG\d+" gene))))


(defn papaemmanuil-gene-names
  [gene-str, mutation-map]
  (let [gene (some->> gene-str (re-find #"([^_]*)_") second)]
    (case gene
      "IDH2" (if-let [variant (some->> gene-str (re-matches #"IDH2_p.(R140|R172).") second)]
               (str gene "-" variant)
               gene)
      "FLT3" (if-let [variant (some->> gene-str (re-matches #"FLT3_(ITD)") second)]
               (str gene "-" variant)
               (if-let [flt3-variant (some->> gene-str (re-matches #"FLT3_p.(D835|I836).*") second)]
                 "FLT3-TKD"
                 "FLT-other"))
      ; CEBPA biallelic und CEBPA monoallelic (1 or 2 alterations per sample), determine in table creation
      "CEBPA" gene
      "MLL" (if (= (:type mutation-map) "PTD")
              "MLL-PTD"
              "MLL")
      ; default case:
      gene)))


(defn cebpa-distinction
  [table-rows] (println "run")
  (let [{:strs [CEBPA] :as mutated-samples-per-gene} (group-by :gene table-rows)
        cebpa-sample-freqs (frequencies CEBPA)
        filter-samples-by-count (fn [cnt, gene-name]
                                  (reduce-kv
                                    (fn [result, sample-map, n]
                                      (cond-> result
                                        (== n cnt) (conj (assoc sample-map :gene gene-name))))
                                    []
                                    cebpa-sample-freqs))
        monoallelic (filter-samples-by-count 1, "CEBPA-monoallelic"),
        biallelic (filter-samples-by-count 2, "CEBPA-biallelic")]
    (-> mutated-samples-per-gene
      (dissoc "CEBPA")
      (assoc
        "CEBPA-monoallelic" monoallelic
        "CEBPA-biallelic" biallelic)
      (->>
        (into (sorted-map))
        vals
        (reduce
          into
          [])))))


(defn papaemmanuil-list->matrix
  [import-list-file, export-matrix-file]
  (csv-supplement-table->gene-sample-table import-list-file, "GENE", "SAMPLE", export-matrix-file,
    {:additional-columns-map {:result "Result", :type "TYPE"}
     :row-filter (fn [{:keys [result]}] (= result "ONCOGENIC"))
     :list-post-processing cebpa-distinction
     :gene-renaming papaemmanuil-gene-names}))


(defn transpose-row-sorted-csv
  [import-csv-file, export-csv-file]
  (let [data (imp/read-csv-file import-csv-file)
        transposed (apply map vector (list* (first data) (sort-by first (rest data))))]
    (with-open [w (io/writer export-csv-file)]
      (csv/write-csv w, transposed, :separator \;))))


(defn read-gene-sample-matrix-csv
  [file]
  (let [rows (imp/read-csv-file file)
        sample-vec (vec (rest (first rows)))
        rows (rest rows),
        entries (reduce
                  (fn [entry-list, row]
                    (let [entry-map {:gene (first row)}]
                      (u/reduce-indexed
                        (fn [entry-list, i, value]
                          (let [sample (nth sample-vec i)]
                            (conj entry-list
                              (assoc entry-map
                                :sample sample
                                :value value))))
                        entry-list
                        (rest row))))
                  []
                  rows)]
    {:samples sample-vec,
     :genes (mapv first rows),
     :entries entries}))


(defn merge-matrix-data-by-sample
  [matrix-1, matrix-2]
  (let [union-data (fn [kw] (vec (mapcat kw [matrix-1, matrix-2])))
        samples (->> (union-data :samples) (map str/trim) distinct sort vec),
        genes (union-data :genes)
        entries (union-data :entries),
        entries-by-gene (group-by :gene entries)]
    (->> genes
      (mapv
        (fn [gene]
          (let [entries (get entries-by-gene gene)]
            ; check one sample entry per gene
            (when-let [sample-duplicates (->> entries (map :sample) frequencies (filter #(->> % second (> 1))) seq)]
              (u/log-error
                (format "The following samples have more than one entry for gene \"%s\": %s"
                  gene
                  (->> sample-duplicates (map first) sort (str/join ", ")))))
            ; determine row
            (let [sample->entry-map (zipmap (map :sample entries) entries)]
              (list* gene
                (mapv
                  (fn [sample]
                    (:value (get sample->entry-map sample)))
                  samples))))))
      (list* (list* "" samples)))))


(defn merge-papaemmanuil-mutations+abnormalities
  [mutations-csv-file, abnormalities-csv-file, result-file]
  (let [merged-rows (merge-matrix-data-by-sample
                      (read-gene-sample-matrix-csv mutations-csv-file),
                      (read-gene-sample-matrix-csv abnormalities-csv-file))]
    (with-open [w (io/writer result-file)]
      (csv/write-csv w, merged-rows, :separator \;))))

(comment
  ; Lindsley, after extraction via: java -jar tabula-0.9.1-jar-with-dependencies.jar -g -r -p 28-38 -f CSV -o linsley.csv Lindsley_Blood_2015_AML\ type-Supplement.pdf
  (csv-supplement-table->gene-sample-table "datasets/lindsley-table.csv" "Gene" "UPN" "lindsley-original.csv")

  ; Genomic Landscape
  (csv-supplement-table->gene-sample-table "datasets/GenomicLandscape-table.tsv" "gene_name" "UPN" "GenomicLandscape.csv"
    {:row-filter gene-name?})
  ; #genes = 5932 #samples = 197 #alterations = 12234

  ; after extraction via: tabula 1.1.1 und manual correction in libreoffice calc
  (papaemmanuil-list->matrix "datasets/papaemmanuil-table-s5-driver-gene-mutations.csv", "papaemmanuil-mutations.csv")
  (transpose-row-sorted-csv "datasets/papaemmanuil-table-s6-chromosomal-abnormalities.csv" "chromosomal-abnormalities.csv")
  (merge-papaemmanuil-mutations+abnormalities "papaemmanuil-mutations.csv" "papaemmanuil-chromosomal-abnormalities.csv" "papaemmanuil-all-data.csv")


  (csv-supplement-table->gene-sample-table "datasets/Age-Related Clonal Hematopoiesis Associated with Adverse Outcomes - Mutation Table.csv"
    "Gene" "ID" "age-related-clonal.csv")

  (csv-supplement-table->gene-sample-table "datasets/comprehensive genomic profiles of small cell lung cancer.csv"
    "Gene_Hugo", "PAT_ID", "small-cell-lung-cancer.csv")

  )