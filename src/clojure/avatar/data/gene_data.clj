; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.gene-data
  (:require
    [avatar.util :as u]
    [clojure.data.int-map :as im]
    [avatar.algorithms.common :as c]
    [clojure.string :as str]
    [avatar.algorithms.score :as score]))



(defn filter-gene-indices
  [pred, {:keys [gene-list] :as gene-sample-data}]
  (persistent!
    (u/reduce-indexed
      (fn [gene-indices, index, gene]
        (cond-> gene-indices
          (pred gene)
          (conj! index)))
      (transient (im/dense-int-set))
      gene-list)))


(defn ->every-alteration-type?
  "Returns a predicate that checks whether the given predicate holds for all of the specified alteration types."
  [predicate-fn, alteration-types]
  (fn check [{:keys [alteration-data] :as gene-data}]
    (reduce
      (fn [result, alteration-type]
        (if (predicate-fn (get alteration-data alteration-type))
          result
          ; short-circuiting false
          (reduced false)))
      true
      alteration-types)))


(defn ->some-alteration-type?
  "Returns a predicate that checks whether the given predicate holds for some of the specified alteration types."
  [predicate-fn, alteration-types]
  (fn check [{:keys [alteration-data] :as gene-data}]
    (reduce
      (fn [result, alteration-type]
        (if (predicate-fn (get alteration-data alteration-type))
          ; short-circuiting true
          (reduced true)
          result))
      false
      alteration-types)))


(defn find-genes-without-alterations
  [alteration-types, gene-sample-data]
  (filter-gene-indices
    (->every-alteration-type?
      (fn [{:keys [alterations]}]
        (zero? (count alterations)))
      alteration-types),
    gene-sample-data))


(defn find-samples-without-clinical-data
  [{:keys [clinical-data-map, sample-count] :as gene-sample-data}]
  (persistent!
    (reduce
      (fn [result-set, sample-index]
        (cond-> result-set (empty? (get clinical-data-map sample-index)) (conj! sample-index)))
      (transient #{})
      (range sample-count))))


(defn find-genes-in-alteration-count-range
  [alteration-types, ^long min-alterations, ^long max-alterations, gene-sample-data]
  (filter-gene-indices
    (->every-alteration-type?
      (fn [{:keys [alterations]}]
        (<= min-alterations (count alterations) max-alterations))
      alteration-types),
    gene-sample-data))


(defn find-genes-outside-alteration-count-range
  [alteration-types, ^long min-alterations, ^long max-alterations, gene-sample-data]
  (filter-gene-indices
    (->some-alteration-type?
      (fn [{:keys [alterations]}]
        (let [n (count alterations)]
          (or
            (< n min-alterations)
            (< max-alterations n))))
      alteration-types),
    gene-sample-data))


(defn find-samples-without-alterations
  [alteration-types, {:keys [gene-list, sample-count] :as gene-sample-data}]
  (let [samples-with-alterations (->> alteration-types
                                   (mapv
                                     (fn [alteration-type]
                                       ; determine samples with alterations for the given type
                                       (->> gene-list
                                         (mapv #(get-in % [:alteration-data, alteration-type, :alterations]))
                                         (reduce im/union (im/dense-int-set)))))
                                   ; samples with alterations in any given type
                                   (reduce im/union))]
    ; determine samples without alterations in all alteration types
    (im/difference (c/populated-dense-int-set sample-count) samples-with-alterations)))


(defn deletion-renaming-permuation
  [^long sample-count, samples-to-delete]
  (let [samples-to-delete (set samples-to-delete)]
    (loop [old-sample-index 0, new-sample-index 0, sample-renaming (transient [])]
      (if (< old-sample-index sample-count)
        (if (contains? samples-to-delete old-sample-index)
          (recur (unchecked-inc old-sample-index), new-sample-index, (conj! sample-renaming nil))
          (recur (unchecked-inc old-sample-index), (unchecked-inc new-sample-index), (conj! sample-renaming new-sample-index)))
        (persistent! sample-renaming)))))


(defn delete-gene-alterations
  "Create a new bitset where the alterations of the samples are shifted according to the given renaming map.
  Alterations of samples that do not occur in the renaming map are discarded."
  [alteration-set, sample-renaming-vec]
  (persistent!
    (reduce
      (fn [result-bitset, old-sample-index]
        (if-let [new-sample-index (nth sample-renaming-vec old-sample-index)]
          (conj! result-bitset new-sample-index)
          result-bitset))
      (transient (im/dense-int-set))
      alteration-set)))


(defn delete-alterations
  "Deletes the alterations and rearranges the alteration according to the given sample renaming."
  [{:keys [alteration-type-set] :as gene-sample-data}, sample-renaming-vec]
  (update-in gene-sample-data [:gene-list]
    (fn [gene-list]
      (persistent!
        (reduce
          (fn [new-gene-list, gene-data]
            (conj! new-gene-list
              ; delete from every alteration type
              (reduce
                (fn [gene-data, alteration-type]
                  (update-in gene-data [:alteration-data, alteration-type]
                    #(-> %
                       (update-in [:alterations] delete-gene-alterations sample-renaming-vec)
                       (update-in [:missing-values] delete-gene-alterations sample-renaming-vec))))
                gene-data
                alteration-type-set)))
          (transient [])
          gene-list)))))


(defn delete-samples-from-permutation
  [{{:keys [column->sample]} :sample-permutation, :as gene-sample-data}, sample-renaming-vec]
  (let [new-column->sample (persistent!
                             (reduce
                               (fn [new-column->sample, old-sample-index]
                                 (if-let [new-sample-index (nth sample-renaming-vec old-sample-index)]
                                   ; rename sample in the current column
                                   (conj! new-column->sample new-sample-index)
                                   ; "delete" sample from the column - the column wil be taken by the next not delete sample
                                   new-column->sample))
                               (transient [])
                               column->sample))]
    (-> gene-sample-data
      (update-in [:sample-permutation] assoc :column->sample new-column->sample, :sample->column (u/inverse-permutation new-column->sample))
      (assoc :sample-count (count new-column->sample)))))


(defn delete-samples-from-map
  [gene-sample-data, map-attribute-kw, sample-renaming-vec]
  (update-in gene-sample-data [map-attribute-kw]
    (fn [old-sample-map]
      (persistent!
        (u/reduce-indexed
          (fn [new-sample-map, old-sample-index, new-sample-index]
            ; if not deleted, rename sample in group map
            (cond-> new-sample-map
              new-sample-index
              (assoc! new-sample-index (get old-sample-map old-sample-index))))
          (transient {})
          sample-renaming-vec)))))


(defn delete-samples-from-vector
  [gene-sample-data, vector-attribute-kw, sample-renaming-vec]
  (let [n (reduce #(cond-> %1 %2 inc) 0 sample-renaming-vec)]
    (update-in gene-sample-data [vector-attribute-kw]
      (fn [old-sample-vector]
        (persistent!
          (u/reduce-indexed
            (fn [new-sample-vector, old-sample-index, new-sample-index]
              ; if not deleted, rename sample in group map
              (cond-> new-sample-vector
                new-sample-index
                (assoc! new-sample-index (get old-sample-vector old-sample-index))))
            (transient (vec (range n)))
            sample-renaming-vec))))))


(defn delete-samples
  [{:keys [sample-count], :as gene-sample-data}, selected-samples]
  (if (seq selected-samples)
    (let [sample-renaming-vec (deletion-renaming-permuation sample-count, selected-samples)]
      (-> gene-sample-data
        (delete-alterations sample-renaming-vec)
        (delete-samples-from-permutation sample-renaming-vec)
        (delete-samples-from-vector :sample-id-vec, sample-renaming-vec)
        (delete-samples-from-map :sample-group-map, sample-renaming-vec)
        (delete-samples-from-map :clinical-data-map, sample-renaming-vec)))
    gene-sample-data))


(defn reduce-over-all-alteration-data
  "Apply the given function in a reduce over all alteration types and all genes for each iteration type.
  The function is called as (f result alteration-type, gene-alteration-map).
  Note, that (reduced something) stops only the reduction for the current alteration type."
  [f, init, {:keys [alteration-type-set, gene-list] :as gene-sample-data}]
  (reduce
    (fn [result, alteration-type]
      (reduce
        (fn [result, gene-data]
          (let [alteration-map (get-in gene-data [:alteration-data, alteration-type])]
            (f result, alteration-type, alteration-map)))
        result
        gene-list))
    init
    alteration-type-set))


(defn group->size-map
  [group-type, {:keys [sample-group-map, gene-list] :as gene-sample-data}]
  (let [group-list (case group-type
                     :sample (vals sample-group-map)
                     :gene (mapv :group gene-list))]
    (persistent!
      (reduce
        (fn [result-map, group]
          (cond-> result-map
            group
            (u/update! group (fnil inc 0))))
        (transient {})
        group-list))))


(defn find-gene-indices
  [gene-list, gene-search-set]
  (let [gene-search-set (if (set? gene-search-set)
                          gene-search-set
                          (set gene-search-set))]
    (persistent!
      (u/reduce-indexed
        (fn [result-vec, index, gene]
          (cond-> result-vec
            (contains? gene-search-set gene)
            (conj! index)))
        (transient [])
        gene-list))))


(defn clinical-attribute-coverage
  "Returns a map that contains a set of sample indices per clinical attribute
  indicating the samples that have values for the corresponding attribute."
  [{:keys [clinical-data-map] :as gene-sample-data}]
  (persistent!
    (reduce-kv
      (fn [coverage-map, sample-index, sample-attribute-map]
        (reduce
          (fn [coverage-map, attribute]
            (u/update-in! coverage-map [attribute]
              (fnil conj (im/dense-int-set))
              sample-index))
          coverage-map
          (keys sample-attribute-map)))
      (transient {})
      clinical-data-map)))


(def ^:const alteration-type-order-map
  (zipmap [:mutation-data, :fusion-data, :expression-data, :methylation-data] (range)))


(def ^:const alteration-type-set (-> alteration-type-order-map keys set))


(defn sort-alteration-types
  [alteration-type-coll]
  (vec
    (sort-by
      alteration-type-order-map
      alteration-type-coll)))


(defn sample-groups
  [{:keys [sample-group-map] :as gene-sample-data}]
  (persistent!
    (reduce-kv
      (fn [group-set, _, group]
        (conj! group-set group))
      (transient #{})
      sample-group-map)))


(defn alteration-type-str
  [alteration-type]
  (-> alteration-type name (str/replace #"-" " ")))


(defn gene-count
  [{:keys [gene-list] :as gene-sample-data}]
  (count gene-list))


(defn sample-count
  [gene-sample-data]
  (:sample-count gene-sample-data))


(defn alteration-contingency-tables
  [{:keys [sample-group-map, gene-list] :as gene-sample-data}]
  (let [grouped-samples (u/inverse-bit-map sample-group-map),
        groups (->> grouped-samples keys sort vec)]
    {:columns (vec (mapcat (fn [group] [(str group " altered") (str group " unaltered")]) groups))
     :rows (mapv
             (fn [{:keys [alterations, gene]}]
               {:gene gene,
                :contingency-table (persistent!
                                     (reduce
                                       (fn [gene-row, group]
                                         (let [samples (get grouped-samples group)
                                               altered (count (im/intersection alterations, samples))
                                               unaltered (- (count samples) altered)]
                                           (-> gene-row
                                             (conj! altered)
                                             (conj! unaltered))))
                                       (transient [])
                                       groups))})
             gene-list)}))


(defn sample-group-size
  "Determine the number of samples in the given group.
  If no group is specified, the total number of samples is returned (assuming that all samples have a key in the given map)."
  [sample-group-map, select-only-for-sample-group]
  (if select-only-for-sample-group
    (->> sample-group-map vals (filter #(= % select-only-for-sample-group)) count)
    (count sample-group-map)))


(defn remove-genes-with-less-alterations
  [^long minimum-alteration-count, gene-list]
  (filterv
    (fn [{:keys [alterations]}]
      (>= (count alterations) minimum-alteration-count))
    gene-list))


(defn fixed?
  [{:keys [order-fixed?] :as gene-data}]
  order-fixed?)


(defn sample-count-in-group
  ^long [sample-group, sample-group-map]
  (if sample-group
    (reduce-kv
      (fn [n, _, group]
        (cond-> n (= group sample-group) inc))
      0
      sample-group-map)
    (count sample-group-map)))


(defn relative-coverage
  ^double [alteration-type, sample-group, {:keys [gene-list, sample-group-map] :as gene-sample-data}]
  (let [gene-list (->> gene-list
                    (c/select-alterations-in-gene-list alteration-type)
                    (c/maybe-remove-sample-alterations-of-other-groups sample-group, sample-group-map))
        max-coverage (sample-count-in-group sample-group, sample-group-map)]
    (/
      (score/total-coverage :alterations, gene-list)
      (double max-coverage))))


(defn mean-overlap
  ^double [alteration-type, sample-group, {:keys [gene-list, sample-group-map] :as gene-sample-data}]
  (let [gene-list (->> gene-list
                    (c/select-alterations-in-gene-list alteration-type)
                    (c/maybe-remove-sample-alterations-of-other-groups sample-group, sample-group-map))
        sample-count (sample-count-in-group sample-group, sample-group-map)]
    (/
      (score/total-overlap :alterations, gene-list)
      (double sample-count))))


(defn gene-coverage-in-snapshot
  [alteration-type, snapshot]
  (let [{:keys [gene-list, sample-count, alteration-type-set]} (:data snapshot)]
    (if (contains? alteration-type-set alteration-type)
      (persistent!
        (reduce
          (fn [result-map, {:keys [gene, alteration-data] :as gene-data}]
            (assoc! result-map
              gene
              (-> (get-in alteration-data [alteration-type, :alterations])
                count
                (* 100.0)
                (/ sample-count))))
          (transient {})
          gene-list))
      (u/key-value-map :gene (constantly Double/NaN) gene-list))))



(defn objective-text
  [{:keys [objective, samples]}]
  (format "%s (%s)"
    (-> objective name str/capitalize)
    (case samples
      :optimization-group "group"
      :all "all")))

(defn objective-in-solution
  [selected-alteration-type, solutions, {:keys [objective, samples] :as objective-spec}]
  (let [objective-fn (case objective
                       :coverage (comp
                                   #(some->> % (* 100.0) (format "%.1f%%"))
                                   (partial relative-coverage selected-alteration-type))
                       :overlap (comp
                                  #(some->> % (format "%.2f"))
                                  (partial mean-overlap selected-alteration-type)))]
    (persistent!
      (reduce
        (fn [row-map, {:keys [id, gene-sample-data, optimization-sample-group] :as solution}]
          (let [objective-value (if (contains? (:alteration-type-set gene-sample-data) selected-alteration-type)
                                  (objective-fn
                                    (when (= samples :optimization-group) optimization-sample-group)
                                    gene-sample-data)
                                  "N/A")]
            (assoc! row-map id objective-value)))
        (transient
          {:objective-text (objective-text objective-spec)})
        solutions))))


(defn sample-indexes-by-id
  [{:keys [sample-id-vec] :as gene-sample-data}, sample-ids]
  (let [sample-id-set (set sample-ids)]
    (persistent!
      (u/reduce-indexed
        (fn [index-set, index, sample-id]
          (cond-> index-set
            (contains? sample-id-set, sample-id)
            (conj! index)))
        (transient #{})
        sample-id-vec))))


(defn delete-samples-by-id
  [gene-sample-data, sample-ids]
  (delete-samples gene-sample-data, (sample-indexes-by-id gene-sample-data, sample-ids)))