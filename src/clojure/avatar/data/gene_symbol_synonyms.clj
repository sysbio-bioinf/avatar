; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.gene-symbol-synonyms
  (:require
    [clj-http.client :as http]
    [clojure.string :as str]
    [avatar.util :as u]
    [clojure.set :as set]
    [com.stuartsierra.component :as c]
    [clj-jfx.core :as jfx]
    [avatar.ui.dialogs.progress-view :as progress]))



(defn create-symbol->synonyms-map
  [lines]
  (persistent!
    (reduce
      (fn [result-map, line]
        (let [[symbol, synonym-text] (str/split line #"\t"),
              symbol (str/trim symbol)]
          (assoc! result-map
            symbol
            (cond-> #{}
              (not (str/blank? synonym-text))
              (into
                (mapv str/trim (-> synonym-text str/trim (str/split #", "))))))))
      (transient {})
      lines)))


(defn create-synonym->symbols-map
  "Creates a map from a synonym gene symbol to the referred gene symbols.
  Sadly a gene symbol can be synonym to several other gene symbols without being semantically equivalent to them."
  [symbol->synonyms-map]
  (persistent!
    (reduce-kv
      (fn [result-map, symbol, synonym-set]
        (reduce
          (fn [result-map, synonym]
            (u/update-in! result-map [synonym] (fnil conj #{}) symbol))
          result-map
          synonym-set))
      (transient {})
      symbol->synonyms-map)))


(defn distinct-synonyms?
  [symbol->synonyms-map]
  (let [symbol-sets (vec (keys symbol->synonyms-map))
        n (count symbol-sets)
        non-empty-intersections (->> (for [i (range n),
                                           j (range (inc i) n)]
                                       [(nth symbol-sets i), (nth symbol-sets j)])
                                  (keep
                                    (fn [[symbol-1, symbol-2]]
                                      (let [synonyms-1 (get symbol->synonyms-map symbol-1),
                                            synonyms-2 (get symbol->synonyms-map symbol-2),
                                            common-element (or
                                                             (some #(contains? synonyms-1 %) synonyms-2)
                                                             (some #(contains? synonyms-2 %) synonyms-1))]
                                        (when common-element
                                          {:symbols [symbol-1, symbol-2]
                                           :common-element common-element})))))]
    (if (seq non-empty-intersections)
      (do
        (println (reduce (fn [n, e] (println n) (inc n)) 0 non-empty-intersections))
        false)
      true)))


(defn debug-synonym-data
  [lines]
  (let [symbol->synonyms-map (create-symbol->synonyms-map lines)
        synonym->symbols-map (create-synonym->symbols-map symbol->synonyms-map)]
    (println "#symbols =" (count symbol->synonyms-map) "#synonyms =" (count synonym->symbols-map)
      "#both =" (count (set/intersection (set (keys symbol->synonyms-map)) (set (keys synonym->symbols-map)))))
    synonym->symbols-map))


(defn download-gene-symbol-synonyms
  "Returns a map from synonym symbols to the corresponding sets of symbols.
  Sadly a gene symbol can be synonym to several other gene symbols without being semantically equivalent to them."
  []
  (let [{:keys [status, body, error]} (http/get
                                        "https://www.genenames.org/cgi-bin/download?col=gd_app_sym&col=gd_aliases&status=Approved&status_opt=2&where=&order_by=gd_app_sym_sort&format=text&limit=&hgnc_dbtag=on&submit=submit")]
    (cond
      error
      (throw error)

      (= status 200)
      (let [lines (str/split-lines (str/trim body))]
        (if (every? #(re-matches #"[^\t]+\t[^\t\n]*?" %) lines)
          (let [symbol->synonyms-map (create-symbol->synonyms-map lines)]
            {:symbol->synonyms symbol->synonyms-map,
             :synonym->symbols (create-synonym->symbols-map symbol->synonyms-map)})
          (u/runtime-exception "The server www.genenames.org did not return a table in text format (tab separated).")))

      ; error from server
      :error
      (u/runtime-exception "Error %s during download query to www.genenames.org." status))))



(defrecord SynonymService [gene-symbol-synonym-data]

  c/Lifecycle

  (start [this]
    this)

  (stop [this]
    this))


(defn create-synonym-service
  []
  (map->SynonymService {:gene-symbol-synonym-data (atom nil)}))


(defn get-gene-symbol-synonym-data
  [{:keys [gene-symbol-synonym-data] :as synonym-service}]
  (or
    (deref gene-symbol-synonym-data)
    (when-let [result-map (try
                            (progress/with-shown-indeterminate-progress "Downloading Gene Symbol Synonym Data ..."
                              (download-gene-symbol-synonyms))
                            (catch Throwable t
                              (u/log-exception "Gene Symbol Synonym Download failed!", t)
                              (jfx/show-error "Error: Gene Symbol Synonym Download",
                                "Could not download gene symbol synonym data from http://www.genenames.org!",
                                (.getMessage t))
                              nil))]
      (reset! gene-symbol-synonym-data result-map)
      result-map)))