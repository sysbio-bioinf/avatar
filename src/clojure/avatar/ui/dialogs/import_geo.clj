; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.import-geo
  (:require
    [clj-jfx.core :as jfx]
    [avatar.util :as u]
    [avatar.ui.tools :as t]
    [clojure.string :as str]
    [avatar.data.geo :as geo]
    [clojure.java.io :as io]
    [avatar.ui.dialogs.import-bioportal :as imp-bp]
    [avatar.data.import :as imp]
    [avatar.data.z-score :as z]
    [avatar.ui.dialogs.progress-view :as progress]
    [frost.quick-freeze :as qf]
    [avatar.data.bioportal :as bp]
    [clj-jfx.table :as table]
    [clj-jfx.properties :as props]
    [avatar.ui.dialogs.import-table-file :as import-table-dialog]
    [avatar.ui.dialogs.import-matrix-file :as import-matrix-dialog])
  (:import
    (javafx.scene.control TextField Button Label TextArea ListView)
    (javafx.scene.layout Region)))




(defn value-count-map
  [columns, rows]
  (persistent!
    (u/reduce-indexed
      (fn [result-map, index, col]
        (assoc! result-map
          col
          (transduce
            (comp
              (keep #(u/safe-nth % index))
              (distinct))
            (completing
              (fn [n, _]
                (inc n)))
            0
            rows)))
      (transient {})
      columns)))




(defn gene-symbol?
  [s]
  (= (u/normalize-str s) "gene symbol"))


(defn gene-something?
  [s]
  (boolean (re-find #"gene" (u/normalize-str s))))


(defn compare-platform-columns
  [{column-1 :entity, n-1 :info}, {column-2 :entity, n-2 :info}]
  (cond
    (gene-symbol? column-1) -1
    (gene-symbol? column-2) 1
    (gene-something? column-1) -1
    (gene-something? column-2) 1
    :else (let [delta (- n-2 n-1)] ; compare whether n-1 is larger than n-2
            (if-not (zero? delta)
              delta
              ; default case: compare normalized names
              (compare (u/normalize-str column-1), (u/normalize-str column-2))))))


(defn show-column-values
  [columns, rows, selected-column]
  (let [column-index (u/some-index #(= % selected-column) columns),
        values (into (sorted-set)
                 (comp
                   (keep #(u/safe-nth % column-index))
                   (remove str/blank?))
                 rows)]
    (jfx/show-and-wait
      (jfx/dialog (format "Values of column \"%s\"" selected-column)
        (ListView. (jfx/observable-array-list values))
        {:modal? true,
         :cancel-button {:text "Close"}
         :width 400}))))


(defn platform-mapping
  [directory, platform-id]
  (let [platform-file (io/file directory, (str platform-id ".txt"))
        mapping-file (io/file directory, (str platform-id ".map"))]
    (when-not (and
                (u/file-exists? platform-file)
                (jfx/ask-confirmation "Import Data from Gene Expression Omnibus",
                  (format "Do you want to use the previously downloaded platform mapping (%s)?"
                    platform-id),
                  (format "The file has been downloaded previously to the following location:\n%s"
                    (str platform-file))))
      (bp/ensure-directory directory)
      (progress/with-shown-indeterminate-progress (format "Downloading gene symbol mapping for platform %s ..." platform-id)
        (u/download-file
          (format
            "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?targ=self&acc=%s&form=text&view=full"
            platform-id),
          platform-file)
        (if-let [[columns & rows] (geo/gene-symbol-mapping-data platform-file)]
          (when-let [selected-column (jfx/select-entity-from-table
                                       (->> (value-count-map columns, rows)
                                         (mapv (partial zipmap [:entity, :info]))
                                         (sort compare-platform-columns)),
                                       {:title (format "Select name column (platform %s)" platform-id),
                                        :description "Select the column containing the names which the ids will be mapped to.",
                                        :entity-name "platform column",
                                        :info-name "value count",
                                        :select-first? true,
                                        :inspect (partial show-column-values columns, rows)})]
            (let [mapping (geo/create-mapping columns, rows, selected-column)]
              (qf/quick-file-freeze mapping-file, mapping))
            #_(qf/quick-file-freeze mapping-file, mapping-data))
          (u/runtime-exception "There is no mapping data in the platform file \"%s\".", (u/file-name platform-file)))
        #_(platform-file->mapping-file platform-file, mapping-file)))
    (when (u/file-exists? mapping-file)
      (qf/quick-file-defrost mapping-file))))


(defn create-table-data
  [note-label, {:keys [sample-ids, values-per-gene] :as expression-data}]
  (let [column-count (inc (count sample-ids))
        [row-count, note-message] (import-table-dialog/display-row-count
                                    {:column-count column-count, :row-count (inc (count values-per-gene))})
        sorted-gene-vec (->> values-per-gene (sort-by :hugo-symbol) (take row-count) vec)]
    (props/set-property note-label, :text, note-message)
    {:columns (into [{:attribute :row-index, :width 50, :cell-factory import-matrix-dialog/row-index-cell-factory}]
                (mapv
                  (fn [column-index]
                    {:attribute column-index, :name (t/excel-letters column-index)
                     :min-width Region/USE_COMPUTED_SIZE, :max-width 150
                     :font {:weight :bold}})
                  (range column-count)))
     :rows (persistent!
             (u/reduce-indexed
               (fn [result-vec, index, {:keys [hugo-symbol, values]}]
                 (conj! result-vec
                   (into {:row-index (+ index 2)}
                     (zipmap (range) (list* hugo-symbol, values)))))
               (transient [(into {:row-index 1} (zipmap (range) (list* "" sample-ids)))])
               sorted-gene-vec))}))


(defn geometric-mean-array
  [array-vec]
  (let [m (count array-vec),
        m-inv (/ 1.0 m),
        n (alength ^doubles (first array-vec))]
    (loop [i 0, result-array (double-array n)]
      (if (< i n)
        (let [mean (loop [j 0, product 1.0]
                     (if (< j m)
                       (let [^doubles a_j (nth array-vec j)
                             a_ji (aget a_j i)]
                         (recur
                           (unchecked-inc j),
                           (* product a_ji)))
                       (Math/pow product, m-inv)))]
          (recur
            (unchecked-inc i),
            (doto result-array (aset-double i mean))))
        result-array))))


(defn geometric-mean-values-per-gene
  [{:keys [values-per-gene] :as expression-data}]
  (assoc expression-data
    :values-per-gene
    (->> values-per-gene
      (group-by :hugo-symbol)
      (reduce-kv
        (fn [result-vec, hugo-symbol, group]
          (conj! result-vec
            (if (== (count group) 1)
              (first group)
              {:hugo-symbol hugo-symbol
               :values (geometric-mean-array (mapv :values group))})))
        (transient []))
      persistent!
      (sort-by :hugo-symbol)
      vec)))


(defn expression-data->gene-sample-data
  [thresholds, study-id, clinical-data-map, clinical-attributes, {:keys [sample-ids] :as expression-data}]
  (let [expressed?-fn (imp-bp/create-expressed?-fn thresholds)
        sample-id->index-map (zipmap sample-ids (range)),
        sample-count (count sample-ids),
        expressions-per-gene (imp/alteration-matrix-gene-alterations-per-sample expressed?-fn, sample-id->index-map, expression-data),
        gene-list (persistent!
                    (u/reduce-kv-indexed
                      (fn [gene-list, index, gene-symbol, expression-map]
                        (conj! gene-list
                          {:gene gene-symbol,
                           :gene-id index,
                           :group nil,
                           :order-fixed? false
                           :alteration-data {:expression-data expression-map}}))
                      (transient [])
                      (into (sorted-map) expressions-per-gene)))]
    {:gene-list (vec gene-list),
     :sample-count sample-count,
     :sample-id-vec (vec sample-ids),
     :sample-group-map (zipmap (range sample-count) (repeat sample-count study-id)),
     :alteration-type-set (conj (sorted-set) :expression-data),
     :sample-permutation {:sample->column (vec (range sample-count)),
                          :column->sample (vec (range sample-count))},
     :clinical-attributes clinical-attributes,
     :clinical-data-map (persistent!
                          (reduce-kv
                            (fn [clinical-data-map, sample-id, clinical-data]
                              (assoc! clinical-data-map (get sample-id->index-map sample-id) clinical-data))
                            (transient {})
                            clinical-data-map))}))


(defn show-study-details-dialog
  [{:keys [id, study, authors, summary, expression-data, clinical-data-map, clinical-attributes] :as data}, complete-fn]
  (jfx/run-now
    (let [control (jfx/create-control "avatar/ui/ImportGEOSeriesInfoDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [^Label title-label,
                  ^Label
                  authors-label,
                  ^TextArea
                  summary-textarea
                  value-table,
                  note-label,
                  import-button,
                  cancel-button]} (jfx/control-children control)
          window (doto (jfx/modal-window (format "Import Data from Gene Expression Omnibus (%s)" id), control-node)
                   (t/add-avatar-icon))]

      (jfx/property-value! title-label, :text, study)
      (jfx/property-value! authors-label, :text, authors)
      (jfx/property-value! summary-textarea, :text, summary)


      (table/setup-table value-table, [], {})

      (props/set (table/data-property value-table)
        (create-table-data note-label, expression-data))

      (jfx/handle-event! import-button, :action
        (fn [_]
          (u/safe-future
            (let [expression-data (-> expression-data
                                    geometric-mean-values-per-gene
                                    z/values->zscores)
                  thresholds (deref (imp-bp/ask-for-expression-thresholds expression-data)),
                  gene-sample-data (progress/with-shown-indeterminate-progress "Importing alteration data ..."
                                     (expression-data->gene-sample-data thresholds, id, clinical-data-map, clinical-attributes, expression-data))]
              (complete-fn gene-sample-data)
              (jfx/close window)))))


      (jfx/handle-event! cancel-button, :action
        (fn [_]
          (jfx/close window)))

      (jfx/show window))))



(defn unzip+import-data
  [file]
  (progress/with-shown-indeterminate-progress "Extracting data ..."
    (let [in (if (= (str/lower-case (u/file-extension file)) "gz")
               (u/gzip-input-stream file)
               (io/input-stream file))]
      (geo/import-gse-file in))))


(defn error-series-matrix-not-found
  [gse-id, t]
  (let [msg (format "Could not download series matrix from GEO for \"%s\"!" gse-id)]
    (u/log-exception msg, t)
    (jfx/show-custom-alert
      {:alert-type :error,
       :title "Import Data from Gene Expression Omnibus",
       :header msg,
       :content (doto (jfx/vbox
                        (jfx/textflow
                          (jfx/text-control "Either the specified GEO Accession does not exist or has multiple platforms.")
                          (jfx/text-control "You can check the number of platforms at GEO and download the desired file manually."))
                        (-> (jfx/hbox
                              (jfx/hyperlink "GEO information page" (geo/series-info-url gse-id))
                              (jfx/hyperlink "Download folder" (geo/series-folder-url gse-id)))
                          (jfx/alignment! :center)
                          (jfx/spacing! 10))
                        (jfx/vbox-margin!
                          (Label. "Otherwise, check whether your computer is connected to the internet.")
                          {:top 5}))
                  (jfx/pref-width! 400))})))


(defn download+import-platform-mapping
  [download-directory, geo-id, platform-id]
  (try
    (platform-mapping download-directory, platform-id)
    (catch Throwable t
      (let [msg (format "Failed to download platform mapping \"%s\" for dataset \"%s\".",
                  platform-id, geo-id)]
        (u/log-exception msg, t)
        (jfx/show-information "Import Data from Gene Expression Omnibus",
          msg,
          "Platform IDs are used instead of gene symbols.")
        nil))))


(defn load-data
  [download-directory, {:keys [data-source, geo-id, file]}]
  (when-let [file (if (= data-source :local-file)
                    file
                    (try
                      (geo/download-series-matrix download-directory, geo-id)
                      (catch Throwable t
                        (error-series-matrix-not-found geo-id, t)
                        nil)))]
    (when-let [data (try
                      (or
                        (unzip+import-data file)
                        (do
                          (jfx/show-information "Import Data from Gene Expression Omnibus",
                            (format "The series matrix for study %s does not contain any data!" geo-id)
                            nil)
                          nil))
                      (catch Throwable t
                        (let [msg (format "Data import from file \"%s\" failed!" (str file))]
                          (u/log-exception msg, t)
                          (jfx/show-error "Import Data from Gene Expression Omnibus",
                            msg,
                            nil)
                          nil)))]
      (let [id->gene-mapping (into {}
                               (keep (partial download+import-platform-mapping download-directory, geo-id))
                               (:platform-ids data))]
        (geo/id->gene-symbols id->gene-mapping, data)))))


(defn show-dialog
  [{:keys [geo-download-directory] :as ui}, complete-fn]
  (jfx/run-now
    (let [dialog-state (atom {})
          control (jfx/create-control "avatar/ui/ImportGEODialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [geo-id-radiobutton,
                  local-file-radiobutton,
                  ^TextField
                  geo-id-texfield
                  ^TextField
                  file-textfield
                  ^Button
                  file-button
                  ^Label
                  error-label
                  ^Button
                  import-button,
                  cancel-button]} (jfx/control-children control)
          window (doto (jfx/modal-window "Import Data from Gene Expression Omnibus", control-node)
                   (t/add-avatar-icon)),
          data-source-prop (jfx/bind (jfx/map-entry-property dialog-state [:data-source])
                             (jfx/selected-toggle-property {:geo-id geo-id-radiobutton, :local-file local-file-radiobutton}))
          error-prop (jfx/map-entry-property dialog-state, [:error])
          geo-id-prop (jfx/map-entry-property dialog-state, [:geo-id])
          file-prop (jfx/map-entry-property dialog-state, [:file])]

      (jfx/bind (jfx/property geo-id-texfield, :disable)
        (jfx/functional-property #(= % :local-file) data-source-prop))

      (let [geo-id?-prop (jfx/functional-property #(= % :geo-id) data-source-prop)]
        (jfx/bind (jfx/property file-textfield, :disable) geo-id?-prop)
        (jfx/bind (jfx/property file-button, :disable) geo-id?-prop))

      (jfx/bind geo-id-prop
        (jfx/property geo-id-texfield, :text))

      (jfx/bind file-prop
        (jfx/property file-textfield, :text))

      (jfx/handle-event! file-button, :action
        (fn [_]
          (when-let [file (jfx/choose-file (t/primary-stage ui), "Select GEO series matrix file.", :open,
                            {:file-types {"series matrix files (*.txt, *.gz)" ["*.txt" "*.gz"]}})]
            (jfx/property-value! file-textfield, :text, (str file)))))

      (jfx/bind (jfx/property error-label, :text) error-prop)

      (jfx/bind error-prop
        (jfx/functional-property
          (fn [data-source, geo-id, file]
            (case data-source
              :geo-id (when-not (some->> geo-id str/trim str/upper-case (re-matches #"GSE\d+"))
                        "You need to specify a valid GEO Accession!")
              :local-file (when-not (u/file-exists? file)
                            "You need to specify an existing series matrix file!")))
          data-source-prop,
          geo-id-prop,
          file-prop))

      (jfx/bind (jfx/property import-button, :disable)
        (jfx/functional-property boolean, error-prop))

      (jfx/handle-event! import-button, :action,
        (fn [_]
          (u/safe-future
            (when-let [data (load-data geo-download-directory, @dialog-state)]
              (jfx/close window)
              (show-study-details-dialog data, complete-fn)))))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      (jfx/show window))))