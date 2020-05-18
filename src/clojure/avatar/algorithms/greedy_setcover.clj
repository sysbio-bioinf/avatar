; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.greedy-setcover
  (:require [avatar.algorithms.greedy-setcover-sort :as new-greedy]
            [clojure.data.int-map :as im]))



(defn find-best-candidate
  [covered-set, candidate-set]
  (let [already-covered (count covered-set)]
    (reduce
      (fn [{:keys [best-candidate, best-coverage-increase, remaining-candidate-set]}, candidate]
        (let [new-covered (count (im/union covered-set, (:alterations candidate))),
              coverage-increase (- new-covered already-covered)]
          (if (> coverage-increase best-coverage-increase)
            ; better candidate found
            {:best-candidate candidate,
             :best-coverage-increase coverage-increase,
             ; keep current best candidate (since we do not know if it wins)
             :remaining-candidate-set (conj remaining-candidate-set candidate)}
            ; candidate is not better
            {:best-candidate best-candidate,
             :best-coverage-increase best-coverage-increase,
             :remaining-candidate-set (cond-> remaining-candidate-set
                                        ; only keep candidates that are able to cover more samples
                                        (pos? coverage-increase) (conj candidate))})))
      {:best-candidate nil, :best-coverage-increase 0, :remaining-candidate-set #{}}
      candidate-set)))


(defn greedy-setcover
  "Greedy Set Cover algorithm that tries to cover all samples with as few as possible genes.
  Overlap is not considered."
  [candidate-list]
  (loop [candidate-set (set candidate-list),
         to-cover-set (new-greedy/determine-cover-set candidate-set),
         covered-set (im/dense-int-set),
         chosen-candidates []]
    (if (or (zero? (count to-cover-set)) (zero? (count candidate-set)))
      chosen-candidates
      (if-let [{:keys [best-candidate,
                       remaining-candidate-set]} (find-best-candidate covered-set, candidate-set)]
        ; candidate found
        (let [covered (:alterations best-candidate)]
          (recur
            (disj remaining-candidate-set best-candidate),
            (im/difference to-cover-set covered),
            (im/union covered-set covered),
            (conj chosen-candidates best-candidate)))
        ; no improving candidate found
        (do (println "no additional candidate found")
            chosen-candidates)))))