; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.bioportal
  (:require
    [clojure.string :as str]
    [clojure.walk :as walk]
    [clj-http.client :as http]
    [clojure.data.json :as json]
    [com.climate.claypoole :as cp]
    [clojure.java.io :as io]
    [avatar.util :as u]
    [clojure.set :as set]
    [com.stuartsierra.component :as c]
    [avatar.ui.dialogs.progress-view :as progress]
    [avatar.ui.download :as dl]
    [avatar.data.csv :as csv]
    [clj-jfx.core :as jfx]
    [avatar.data.z-score :as z])
  (:import (java.net URL)
           (org.apache.commons.compress.archivers.tar TarArchiveEntry)
           (java.nio.file Path)
           (java.util.regex Pattern)))



(def ^:private bioportal-map
  {:cbioportal {:name "cBioPortal for Cancer Genomics",
                :webservice-url "http://www.cbioportal.org/webservice.do",
                :gene-sets-url "http://www.cbioportal.org/portal_meta_data.json"}})


(defn bioportal
  [portal-id & {:keys [cache?, threads]}]
  (when-let [cbp (get bioportal-map portal-id)]
    (cond-> cbp
      true (assoc :pool (cp/threadpool (or threads 1), :daemon true, :name "cbioportal"))
      cache? (assoc :cache (atom {})))))


(defn clear-cache
  [cbioportal]
  (when cbioportal
    (update-in cbioportal [:cache] (fn [cache-atom] (reset! cache-atom {})))))


(defn shutdown
  [cbioportal]
  (doto cbioportal
    (some-> :pool cp/shutdown)
    clear-cache))


(defn gene-sets
  [{:keys [gene-sets-url] :as cbioportal}]
  (let [{:keys [body]} (http/get gene-sets-url)]
    (->> (get (json/read-str body) "gene_sets")
      vals
      (mapv
        (fn [gene-set-map]
          (-> gene-set-map
            walk/keywordize-keys
            (update-in [:gene_list] str/split #" ")))))))



(defn tsv->maps
  [tsv-str]
  (let [lines (->> tsv-str
                ; split into rows
                str/split-lines
                ; remove comments
                (remove #(.startsWith (str/trim %) "#"))
                ; split into columns
                (mapv #(str/split % #"\t")))
        columns (mapv keyword (first lines))]
    (mapv
      #(zipmap columns %)
      (rest lines))))


(defn cbioportal-request
  [{:keys [webservice-url] :as cbioportal} cmd, & param-name+value-pairs]
  (assert (even? (count param-name+value-pairs)) "Parameter name and value pairs expected")
  (let [request-url (str/join "&"
                      (list*
                        (str webservice-url "?cmd=" cmd)
                        (->> param-name+value-pairs
                          (partition 2)
                          (keep (fn [[param, value]] (when value (str param "=" value)))))))
        {:keys [body, status, error] :as result-map} (http/get request-url {:socket-timeout 3000 :conn-timeout 3000})]
    (cond
      ; exception in request? => rethrow
      error
      (throw error)

      (not= status 200)
      (throw (RuntimeException. (format "ERROR %s occured for request %s", status, request-url)))

      (re-matches #"Error:(.|\r|\n)*" body)
      (if (re-find #"Error:\s+No genetic profiles available for:" body)
        (do
          (u/log-debug "cbioportal-request error: %s" body)
          ; return nil to signal no genetic pro
          nil)
        ; throw exception since error is unexpected
        (throw (RuntimeException. (format "ERROR for request %s\n%s", request-url, body))))

      :ok
      (tsv->maps body))))


(defn cbioportal-cached-request
  [{:keys [cache] :as cbioportal}, cmd, & param-name+value-pairs]
  (assert (even? (count param-name+value-pairs)) "Parameter name and value pairs expected")
  (if cache
    (let [cache-path [cmd (apply hash-map param-name+value-pairs)]]
      (if-let [result-data (get-in (deref cache) cache-path)]
        result-data
        (when-let [result-data (apply cbioportal-request cbioportal cmd, param-name+value-pairs)]
          (swap! cache assoc-in cache-path result-data)
          result-data)))
    (apply cbioportal-request cbioportal cmd, param-name+value-pairs)))


(defn cancer-studies
  [cbioportal]
  (->> (cbioportal-cached-request cbioportal, "getCancerStudies")
    (mapv #(set/rename-keys %, {:cancer_study_id :study-id}))))


(defn clean-barcode
  [^String barcode]
  (cond
    (.contains barcode "Tumor") (str/replace barcode, "Tumor", "01")
    (.contains barcode "Normal") (str/replace barcode, "Normal", "11")
    :else barcode))


(defn normalize-id
  "Trims the id and transforms TCGA barcodes to ids analog to what cbioportal does (other ids are kept as is).
  Returns nil for blank ids."
  [id, sample?]
  (when-let [id (u/trim-nil id)]
    (if-not (str/starts-with? id "TCGA")
      id
      (let [barcode (clean-barcode id),
            segments (-> barcode clean-barcode (str/split #"-")),
            id (str/join "-" (take 3 segments))]
        (if sample?
          (or
            (some->> barcode (re-matches #"^(TCGA-\w\w-\w\w\w\w-\d\d).*$") second)
            (str
              id
              "-"
              ; if enough segments, return 4th segment, otherwise default value
              (if (> (count segments) 3) (nth segments 3) "01")))
          id)))))


(defn sample-ids
  [mutation-data]
  (when (seq mutation-data)
    (-> (first mutation-data)
      keys
      set
      (disj :COMMON :GENE_ID)
      sort)))


(defn data-archive-url
  ^URL [study-id]
  (URL.
    (format
      "http://download.cbioportal.org/%s.tar.gz"
      study-id)))


(defn read-csv
  [input-stream]
  (persistent!
    (reduce
      conj!
      (transient [])
      (csv/csv-rows-reducible input-stream, "\t", "\"", "#", false))))


(def normalize-column (comp str/lower-case str/trim))

(defn import-csv
  [input-stream, keyword-column-map]
  (let [csv-data (read-csv input-stream),
        column-index-map (zipmap (->> csv-data first (mapv normalize-column)) (range)),
        keyword-index-map (reduce-kv
                            (fn [m, keyword, column]
                              (if-let [index (get column-index-map (normalize-column column))]
                                (assoc m keyword index)
                                (u/runtime-exception "Could not find column \"%s\"." (normalize-column column))))
                            {}
                            keyword-column-map)]
    (persistent!
      (reduce
        (fn [result-vec, row]
          (conj! result-vec
            (reduce-kv
              (fn [m, kw, index]
                (assoc m kw (nth row index)))
              {}
              keyword-index-map)))
        (transient [])
        (rest csv-data)))))


(defn laml-fusion-format
  [_, fusion-str]
  (when-let [[_ gene-1 _ gene-2] (re-matches #"([^\(]+)(\([+-]\))([^\(]+)(\([+-]\))\s*fusion.*" fusion-str)]
    {:gene-1 gene-1, :gene-2 gene-2}))


(defn simple-pair-fusion-format
  [_, fusion-str]
  (if-let [[_, gene-1, gene-2] (re-matches #"([^\(]+)-([^\(]+)\s[Ff]usion" fusion-str)]
    {:gene-1 gene-1, :gene-2 gene-2}
    (when (re-matches #"[^\(]+-(intragenic)" fusion-str)
      {:fusion fusion-str})))


(defn plain-pair-fusion-format
  [_, fusion-str]
  (when-let [[_, gene-1, gene-2] (re-matches #"([^\(]+)[-_]([^\(]+)" fusion-str)]
    {:gene-1 gene-1, :gene-2 gene-2}))


(defn truncation-format
  [_, fusion-str]
  (when (re-matches #"[^\(]+\s(Truncation|fusion)" fusion-str)
    {:fusion fusion-str}))

(defn indirect-fusion-format
  [original-gene, fusion-str]
  (when-let [match (#{"fusion" "silenced"} (-> fusion-str str/trim str/lower-case))]
    {:fusion (str original-gene " " match)}))


(defn fusion-name
  [original-gene, fusion-str]
  (let [fusion-str (str/trim fusion-str)]
    (let [{:keys [gene-1, gene-2, fusion]} (some
                                             #(% original-gene, fusion-str)
                                             [laml-fusion-format,
                                              simple-pair-fusion-format,
                                              ; after simple pair
                                              plain-pair-fusion-format,
                                              truncation-format,
                                              indirect-fusion-format])]
      (cond
        fusion fusion
        (and gene-1 gene-2) (str gene-1 "-" gene-2)
        (or gene-1 gene-2) (do
                             (u/log-debug "Could not extract both genes from fusion entry. Gene 1: %s Gene 2: %s - Fusion: %s" (pr-str gene-1) (pr-str gene-2) fusion-str)
                             fusion-str)
        :else (do
                (u/log-debug "Fusion entry does not match any pattern: %s" fusion-str)
                fusion-str)))))


(defn import-fusions
  [input-stream]
  (when-let [csv-data (try
                        (import-csv input-stream, {:gene "hugo_symbol", :sample "tumor_sample_barcode", :fusion "fusion"})
                        (catch Throwable t
                          (u/log-exception "Could not import fusion data. Wrong format?" t)))]
    (mapv
      (fn [{:keys [fusion, sample, gene]}]
        {:original-gene gene,
         :fusion? true
         :sample (normalize-id sample, true),
         :gene (fusion-name gene, fusion)})
      csv-data)))


(defn import-mutations
  [input-stream]
  (let [mutation-data (import-csv input-stream, {:gene "hugo_symbol", :sample "tumor_sample_barcode"})]
    (mapv
      (fn [{:keys [gene, sample] :as m}]
        (assoc m
          :original-gene gene,
          :fusion? false,
          :sample (normalize-id sample, true)))
      mutation-data)))


(defn parse-double-value
  ^double [s]
  (try
    (Double/parseDouble s)
    (catch Throwable t
      ; parsing failed - no expression value?
      Double/NaN)))


(defn values-per-gene-array
  [sample-index-set, row]
  (u/reduce-indexed
    (fn [^doubles row-array, column-index, sample-index]
      (doto row-array
        (aset-double column-index (parse-double-value (nth row sample-index)))))
    (double-array (count sample-index-set))
    sample-index-set))


(defn build-value-table
  [gene-symbol-attr, gene-symbol-column, non-sample-column-set, csv-rows]
  (let [columns (first csv-rows),
        column->index (zipmap (mapv normalize-column columns) (range)),
        symbol-index (get column->index gene-symbol-column),
        non-sample-column-indices (set (mapv #(get column->index %) non-sample-column-set)),
        sample-index-set (cond-> (into (sorted-set) (vals column->index))
                           symbol-index (disj symbol-index)
                           (seq non-sample-column-indices) (set/difference non-sample-column-indices)),
        sample-id-vec (mapv #(normalize-id (nth columns %), true) sample-index-set),
        values-per-gene (mapv
                          (fn [row]
                            (cond-> {:values (values-per-gene-array sample-index-set, row)}
                              symbol-index (assoc gene-symbol-attr (nth row symbol-index))))
                          (rest csv-rows))]
    (when (seq values-per-gene)
      {:values-per-gene values-per-gene
       :sample-ids sample-id-vec})))


(defn import-value-table
  [input-stream]
  (build-value-table :hugo-symbol, "hugo_symbol", #{"entrez_gene_id"} (read-csv input-stream)))


(defn import-clinical-data
  [sample?, input-stream]
  (let [id-attribute (if sample? "SAMPLE_ID" "PATIENT_ID"),
        clean-ids (cond-> #(update-in % ["PATIENT_ID"] normalize-id false)
                    sample?
                    (comp #(update-in % ["SAMPLE_ID"] normalize-id true)))
        data-rows (read-csv input-stream),
        column-names (mapv (comp str/upper-case str/trim) (first data-rows))]
    (persistent!
      (reduce
        (fn [clincal-data-map, row]
          (let [row-map (clean-ids (zipmap column-names row))]
            (assoc! clincal-data-map
              (get row-map id-attribute)
              row-map)))
        (transient {})
        (rest data-rows)))))


(defn import-clinical-sample-data
  [input-stream]
  (import-clinical-data true, input-stream))


(defn import-clinical-patient-data
  [input-stream]
  (import-clinical-data false, input-stream))


(def only-file-entries
  (filter
    (fn [{:keys [^TarArchiveEntry entry]}]
      (.isFile entry))))


(defn entry-name-matches
  [name-pattern]
  (let [pattern (cond-> name-pattern (not (instance? Pattern name-pattern)) re-pattern)]
    (filter
      (fn [{:keys [entry-name]}]
        (re-matches pattern, entry-name)))))


(defn extract-metadata
  [{:keys [input-stream, entry-name, ^TarArchiveEntry entry]}]
  (->> input-stream
    io/reader
    line-seq
    (remove str/blank?)
    (reduce
      (fn [metadata-map, line]
        (if-let [[_, attribute, value] (re-matches #"([^:]+):\s*(.*)" line)]
          (assoc metadata-map (str/trim attribute) (str/trim value))
          (do
            (u/log-debug "Line did not match metadata format: %s" (pr-str line))
            metadata-map)))
      {:entry-name entry-name})))


(defn import-data-from-entry
  [entry-name->profile-map, study-data-map, {:keys [input-stream, entry-name]}]
  (if-let [{:keys [profile-id, import-fn] :as profile} (get entry-name->profile-map entry-name)]
    (assoc study-data-map profile-id (import-fn input-stream))
    study-data-map))


(defn import-data-from-archive
  [study-data-archive, selected-profiles]
  (let [entry-name->profile-map (reduce-kv
                                  (fn [result-map, _, {:keys [entry-name] :as profile}]
                                    (assoc result-map entry-name profile))
                                  {}
                                  selected-profiles)]
    (transduce
      (comp
        only-file-entries
        (entry-name-matches #"^data_.*"))
      (completing (partial import-data-from-entry entry-name->profile-map))
      {}
      (u/tar-gz-entries study-data-archive))))


(defn attribute=
  [entry-metadata, attribute, desired-value]
  (some->> attribute
    (get entry-metadata)
    str/upper-case
    (= desired-value)))


(defn mutation-data?
  [entry-metadata]
  (attribute= entry-metadata, "genetic_alteration_type", "MUTATION_EXTENDED"))


(defn fusion-data?
  [entry-metadata]
  (attribute= entry-metadata, "genetic_alteration_type", "FUSION"))


(defn expression-data?
  "Does the metadata describe a file with Z-scores of mRNA expression data?"
  [entry-metadata]
  (and
    (attribute= entry-metadata, "genetic_alteration_type", "MRNA_EXPRESSION")
    (attribute= entry-metadata, "datatype", "Z-SCORE")
    (re-matches #".*_Zscores" (get entry-metadata "stable_id"))))


(defn methylation-data?
  [entry-metadata]
  (attribute= entry-metadata, "genetic_alteration_type", "METHYLATION"))


(defn clinical-sample-data?
  [entry-metadata]
  (and
    (attribute= entry-metadata, "genetic_alteration_type", "CLINICAL")
    (attribute= entry-metadata, "datatype", "SAMPLE_ATTRIBUTES")))


(defn clinical-patient-data?
  [entry-metadata]
  (and
    (attribute= entry-metadata, "genetic_alteration_type", "CLINICAL")
    (attribute= entry-metadata, "datatype", "PATIENT_ATTRIBUTES")))


(defn entry-data-filename
  [entry-metadata]
  (some-> (get entry-metadata "data_filename") str/trim))


(defn archive-path
  ^Path [directory, study-id]
  (.toPath (io/file directory, (format "%s.tar.gz" study-id))))


(def supported-archive-data-profiles
  {:clinical-samples {:predicate clinical-sample-data?,
                      :default-entry-names ["data_clinical.txt" "data_clinical_samples.txt" #".*clinical_data_sample.txt"],
                      :import-fn import-clinical-sample-data}
   :clinical-patients {:predicate clinical-patient-data?,
                       :default-entry-names ["data_clinical_patients.txt" #".*clinical_data_patient.txt"]
                       :import-fn import-clinical-patient-data}
   :mutation-data {:predicate mutation-data?,
                   :default-entry-names ["data_mutations_extended.txt"],
                   :import-fn import-mutations},
   :fusion-data {:predicate fusion-data?,
                 :default-entry-names ["data_fusions.txt"],
                 :import-fn import-fusions}
   :expression-data {:predicate expression-data?,
                     :default-entry-names ["data_RNA_Seq_v2_mRNA_median_Zscores.txt"]
                     :import-fn import-value-table}
   :methylation-data {:predicate methylation-data?,
                      :default-entry-names ["data_methylation_hm27.txt" "data_methylation_hm450.txt"]
                      :import-fn (comp z/values->zscores import-value-table)}})


(defn process-archive-entry
  [possible-profiles, found-profiles-map, {:keys [input-stream, entry-name] :as entry-map}]
  (let [entry-type (second (re-matches #"^(meta|data)_.*", entry-name))]
    ; match both: "meta_*" files to find entry names and "data_*" entry names for default names
    (case entry-type
      "meta" (let [metadata (extract-metadata entry-map),
                   [profile-id matching-profile] (u/some-value-kv
                                                   (fn [_, {:keys [predicate] :as profile}]
                                                     (predicate metadata))
                                                   possible-profiles)
                   entry-name (entry-data-filename metadata)]
               (cond-> found-profiles-map
                 (and matching-profile entry-name)
                 (assoc-in [profile-id, :meta]
                   (assoc matching-profile
                     :entry-name entry-name,
                     :profile-id profile-id))))
      "data" (let [[profile-id matching-profile] (u/some-value-kv
                                                   (fn [_, {:keys [default-entry-names] :as profile}]
                                                     (some
                                                       (fn [search-name]
                                                         (if (instance? Pattern search-name)
                                                           (re-matches search-name entry-name)
                                                           (= search-name entry-name)))
                                                       default-entry-names))
                                                   possible-profiles)]
               (cond-> found-profiles-map
                 matching-profile
                 (assoc-in [profile-id, :default-name]
                   (assoc matching-profile
                     :entry-name entry-name,
                     :profile-id profile-id)))))))


(defn find-profiles-in-archive
  "Find the profiles of the given map of possible profiles contained in a cbioportal data archive."
  [possible-profiles, study-data-archive-path]
  (->> (transduce
         (comp
           only-file-entries
           (entry-name-matches #"^(meta|data)_.*"))
         (completing (partial process-archive-entry possible-profiles))
         {}
         (u/tar-gz-entries study-data-archive-path))
    ; decide between :meta and :default-name (use :meta if available, :default-name otherwise)
    (reduce-kv
      (fn [found-profiles, profile-id, {:keys [meta, default-name]}]
        (assoc found-profiles profile-id (or meta default-name)))
      {})))


(defn ensure-directory
  [directory]
  (let [file (io/file directory)]
    (when-not (.exists file)
      (try
        (.mkdirs file)
        (catch Throwable t
          (jfx/show-error "Import Error", "Could not create download directory."
            (format "Download directory \"%s\" does not exists and needs to be created! Automatic creation failed!" directory))
          nil)))))


(defn download-archive
  [directory, study-id]
  (let [url (data-archive-url study-id)]
    (ensure-directory directory)
    (dl/download-with-progress "Download study", (format "Downloading archive of study %s" study-id)
      (.toFile (archive-path directory, study-id)), url)))






(defrecord Bioportal [bp, study-list, download-directory]

  c/Lifecycle

  (start [this]
    (assoc this
      :bp (bioportal :cbioportal, :cache? true, :threads 4)))

  (stop [this]
    (some-> bp shutdown)
    (assoc this
      :bp nil)))


(defn create-bioportal
  [download-directory]
  (map->Bioportal {:study-list (atom nil)
                   :download-directory download-directory}))


(defn study-list
  [bioportal]
  (if-let [study-list (deref (:study-list bioportal))]
    study-list
    (progress/with-shown-indeterminate-progress "Downloading study list ..."
      (let [study-list (cancer-studies (:bp bioportal))]
        (reset! (:study-list bioportal) study-list)
        study-list))))


(defn reuse-cached-archive?
  [download-directory, study-id]
  (and
    (u/file-exists? (archive-path download-directory, study-id))
    (jfx/ask-confirmation "Import from bioportal",
      "Do you want to use the previously downloaded study archive?",
      (format "The archive has been downloaded previously to the following location:\n%s"
        (archive-path download-directory, study-id)))))


(defn download-study-data
  [{:keys [download-directory] :as bioportal}, study-id]
  (when-not (or
              (reuse-cached-archive? download-directory, study-id)
              (download-archive download-directory, study-id))
    (u/log-error "The selected study (id = %s) has no data archive!" study-id)
    (jfx/show-error "Download Error", (format "The selected study (id = %s) has no data archive!" study-id),
      (format "There is no data archive at: %s" (data-archive-url study-id)))
    :bioportal/DOWNLOAD-ERROR))


(defn archive-profiles
  [{:keys [download-directory] :as bioportal}, study-id]
  (find-profiles-in-archive supported-archive-data-profiles, (.toString (archive-path download-directory, study-id))))


(defn import-archive
  [{:keys [download-directory] :as bioportal}, study-id, selected-profiles]
  (progress/with-shown-indeterminate-progress (format "Extracting data from study archive %s ..." (archive-path nil, study-id))
    (let [archive-path (.toString ^Path (archive-path download-directory, study-id))]
      (import-data-from-archive archive-path, selected-profiles))))