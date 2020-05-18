; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.common
  (:require
    [clojure.data.int-map :as im]
    [avatar.util :as u]
    [clojure.string :as str]))



(defn union-feature-set
  ([set-attribute, candidate-list]
   (union-feature-set (im/dense-int-set), set-attribute, candidate-list))
  ([initial-set, set-attribute, candidate-list]
   (reduce
     im/union
     initial-set
     (mapv
       #(get % set-attribute)
       candidate-list))))


(defn union
  [a, b]
  (cond
    (nil? a) b
    (nil? b) a
    :else (im/union a, b)))



(defn populated-dense-int-set
  [n]
  (into (im/dense-int-set) (range n)))


(defn samples-from-opposite-sample-group
  [sample-count, sample-group, sample-group-map]
  (when sample-group
    ; get samples with specified group
    (let [group-samples (reduce-kv
                          (fn [result-set, sample-index, group]
                            (cond-> result-set (= group sample-group) (conj sample-index)))
                          (im/dense-int-set)
                          sample-group-map)]
      ; only when there are samples in that group
      (when (seq group-samples)
        ; determine samples from other groups to delete
        (im/difference
          (populated-dense-int-set sample-count)
          group-samples)))))


(defn determine-max-sample-count
  [gene-list]
  (inc
    (reduce
      (fn [cnt, {:keys [alterations]}]
        (max
          cnt,
          (reduce max 0 alterations)))
      -1
      gene-list)))


(defn delete-sample-alterations
  [sample-index-set, gene-list]
  (mapv
    (fn [gene-data]
      (update-in gene-data [:alterations] im/difference sample-index-set))
    gene-list))


(defn maybe-remove-sample-alterations-of-other-groups
  "If a sample group is specified, the samples from other groups (according to the given sample group map) will be deleted."
  [sample-group, sample-group-map, gene-list]
  (let [samples-to-delete (samples-from-opposite-sample-group (determine-max-sample-count gene-list), sample-group, sample-group-map)]
    (if samples-to-delete
      (delete-sample-alterations samples-to-delete, gene-list)
      gene-list)))


(defn limit-alterations-to-sample-set
  "Alters the gene list such that only alterations of the given sample set are kept."
  [sample-count, sample-index-set, gene-list]
  (let [to-delete-set (into (im/dense-int-set)
                        (remove #(contains? sample-index-set %))
                        (range sample-count))]
    (mapv
      (fn [gene-data]
        (update-in gene-data [:alterations] im/difference to-delete-set))
      gene-list)))


(defn determine-sample-count
  [gene-list]
  (count
    (union-feature-set :alterations gene-list)))


(def NOVALUE "<NO VALUE>")

(defn attribute-value
  [clinical-attributes-map, attribute]
  (or
    (u/trim-nil (get clinical-attributes-map attribute))
    NOVALUE))


(defn select-alterations-in-gene-list
  "Transforms the gene list to contain only the specified type of alterations per gene."
  [alteration-type, gene-list]
  (if alteration-type
    (mapv
      (fn [gene-data]
        (-> gene-data
          (dissoc :alteration-data)
          (merge (get-in gene-data [:alteration-data, alteration-type]))))
      gene-list)
    (u/illegal-argument "No alteration type given!")))


(defn select-alteration-data
  "Transforms the gene sample data to contain only the specified type of alterations."
  [alteration-type, {:keys [alteration-type-set, gene-list], :as gene-sample-data}]
  (if (contains? alteration-type-set alteration-type)
    (-> gene-sample-data
      (dissoc :alteration-type-set)
      (assoc :selected-alteration-type alteration-type)
      (assoc :gene-list (select-alterations-in-gene-list alteration-type, gene-list)))
    (u/illegal-argument "The alteration type %s is not available in the current data (%s)!"
      alteration-type, (str/join ", " alteration-type-set))))


(defn simple-parameters->algorithm-parameters
  [simple, gene-count]
  (let [{:keys [solution-count,
                selected-genes-count,
                swapped-genes-count,
                solution-combinations-count,
                selection-pressure,
                search-step-count]} simple,
        gene-count (double gene-count)]
    {:algorithm :NSGAII,
     ; initialization
     :population-size solution-count,
     :selection-probability (/ selected-genes-count gene-count),
     ; search step
     :mutation-probability (/ swapped-genes-count gene-count),
     :crossover-probability (/ (double solution-combinations-count) (quot solution-count 2)),
     :tournament-size selection-pressure
     ; computation budget
     :iteration-count search-step-count}))


(defn algorithm-parameter-config
  [{:keys [setup-type, simple, expert] :as setup}, gene-count]
  (case setup-type
    :simple (simple-parameters->algorithm-parameters simple, gene-count)
    :expert expert))
