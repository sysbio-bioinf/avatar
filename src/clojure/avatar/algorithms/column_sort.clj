; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.column-sort
  (:require
    [avatar.util :as u])
  (:import
    (java.util BitSet)))



(defn conj-bit
  [^BitSet bs, ^long bit]
  (doto bs
    (.set bit)))


(defn column-bitsets
  [gene-count, sample-count, sample->column, row-bitsets]
  (let [col-bs-vec (vec (repeatedly sample-count #(BitSet. gene-count)))]
    (u/reduce-indexed
      (fn [col-bs-vec, row-index, row-bs]
        ; for each row bitset, set the row-bit in each column that occurs in the row bitset ("transpose")
        (reduce
          (fn [col-bs-vec, sample-index]
            (let [column-index (sample->column sample-index)]
              (update-in col-bs-vec [column-index] conj-bit
                ; we invert the row-indices to achieve the intended lexicographic order
                ; (due to little endian encoding in BitSet.toLongArray)
                (- gene-count 1 row-index))))
          col-bs-vec
          row-bs))
      col-bs-vec
      row-bitsets)))


(defn get-value
  ^long [bsv, ^long i]
  (if (>= i (count bsv))
    0
    (nth bsv i)))


(defn score
  "Convert bitset to a big integer corresponding to the bit representation of the bitset."
  [^BitSet bs]
  (let [bytes (.toByteArray bs)
        n (alength bytes)]
    (loop [i (unchecked-dec n), score (BigInteger/valueOf 0)]
      (if (>= i 0)
        (let [x (long (aget bytes i))
              x (cond-> x (neg? x) (+ 256))]
          (recur (unchecked-dec i), (+ (* score 256) x)))
        score))))


(defn lexicographic-sort
  [group-first?,
   ignore-fixed-genes?,
   alteration-type,
   {:keys [gene-list, sample-count, sample-group-map, alteration-type-set],
    {:keys [sample->column, column->sample]} :sample-permutation,
    :as gene-sample-data}]
  (assert (contains? alteration-type-set alteration-type) "Data must have alteration data of the specified type.")
  (let [gene-list (cond->> gene-list
                    ignore-fixed-genes? (u/removev :order-fixed?))
        gene-count (count gene-list),
        group->order-index (zipmap
                             (->> (range sample-count) (map (comp #(get sample-group-map %) column->sample)) distinct)
                             (range)),
        scored-columns (->> gene-list
                         (mapv #(get-in % [:alteration-data, alteration-type, :alterations]))
                         ; create "transposed" bitsets to compare columns base on rows with alterations
                         (column-bitsets gene-count, sample-count, sample->column)
                         ; convert bitsets to vectors of long numbers for lexicographic comparison (most significant number is at the end of the vector)
                         (mapv
                           (fn [column-index, bitset]
                             (let [sample (column->sample column-index)]
                               {:sample-index sample, :group (get sample-group-map sample) :score (score bitset)}))
                           (range)))
        comparator (if group-first?
                     (fn group-first-comparator
                       [{group-1 :group, score-1 :score}, {group-2 :group, score-2 :score}]
                       (let [index-1 (group->order-index group-1),
                             index-2 (group->order-index group-2)]
                         (if (= index-1 index-2)
                           (> score-1 score-2)
                           (< index-1 index-2))))
                     (fn score-first-comparator
                       [{group-1 :group, score-1 :score}, {group-2 :group, score-2 :score}]
                       (if (= score-1 score-2)
                         (< (group->order-index group-1) (group->order-index group-2))
                         (> score-1 score-2))))
        new-column->sample (->> scored-columns
                             (sort comparator)
                             (mapv :sample-index))]
    (update-in gene-sample-data [:sample-permutation]
      assoc :column->sample new-column->sample :sample->column (u/inverse-permutation new-column->sample))))