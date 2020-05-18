; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.score
  (:require
    [clojure.data.int-map :as im]
    [avatar.algorithms.common :as c]
    [avatar.util :as u]))



(defn total-overlap+coverage
  "Calculates the total overlap counting covered samples multiple times.
  Returns both overlap and coverage."
  [set-attribute, selected-genes-set]
  (if (seq selected-genes-set)
    (let [selected-sample-sets (mapv #(get % set-attribute) selected-genes-set),
          n (count selected-sample-sets),
          element-count-sum (loop [i 0, sum 0]
                              (if (< i n)
                                (let [sample-set (nth selected-sample-sets i)]
                                  (recur
                                    (unchecked-inc i),
                                    (unchecked-add sum (count sample-set))))
                                sum)),
          coverage (count (reduce im/union selected-sample-sets))]
      {:overlap (- element-count-sum coverage),
       :coverage coverage})
    {:overlap 0,
     :coverage 0}))


(defn total-overlap
  "Calculates the total overlap counting covered samples multiple times."
  ^long [set-attribute, selected-genes-set]
  (:overlap (total-overlap+coverage set-attribute, selected-genes-set)))


(defn total-coverage
  ^long [set-attribute, selected-genes-set]
  (if (seq selected-genes-set)
    (let [selected-sample-sets (mapv #(get % set-attribute) selected-genes-set)]
      (count (reduce im/union selected-sample-sets)))
    0))


(defn max-coverage
  ^long [set-attribute, candidate-list]
  (count (c/union-feature-set set-attribute, candidate-list)))


(defn cover-count-sum
  ^long [set-attribute, candidate-vec]
  (let [n (count candidate-vec)]
    (loop [i 0, sum 0]
      (if (< i n)
        (let [gene-set (get (nth candidate-vec i) set-attribute)]
          (recur (unchecked-inc i), (unchecked-add sum, (count gene-set))))
        sum))))


(defn relative-coverage
  ^double [^long max-coverage, ^long coverage]
  (/
    coverage
    (double max-coverage)))


(defn overlap
  ^long [^long coverage, ^long overlap-count]
  (- overlap-count coverage))


(defn relative-overlap
  ^double [^long max-overlap, ^long coverage, ^long overlap-count]
  (/
    (overlap coverage, overlap-count)
    (double max-overlap)))

