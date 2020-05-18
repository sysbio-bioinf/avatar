; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.greedy-setcover-sort
  (:require [avatar.algorithms.common :as c]
            [clojure.data.int-map :as im]
            [avatar.util :as u]
            [clojure.set :as set]
            [avatar.algorithms.score :as score]
            [clojure.data :as data]))



(definterface IObjectiveFunction
  (^double calculate [covered-set, ^long overlap-count, candidate-set])
  (^double weighting [])
  (create [^double weighting, ^long max-coverage, ^long max-overlap, ^long additional-overlap]))


(defn objective-fn?
  [x]
  (instance? IObjectiveFunction x))


(defn calculate
  #_{:inline (fn [objective-fn, covered-set, overlap-count, candidate-set]
               `(let [^IObjectiveFunction obj-fn# ~objective-fn]
                  (.calculate obj-fn#, ~covered-set, ~overlap-count, ~candidate-set)))}
  ^double [objective-fn, covered-set, ^long overlap-count, candidate-set]
  (.calculate ^IObjectiveFunction objective-fn, covered-set, overlap-count, candidate-set))



(deftype WeightedMaxCoverageMinOverlapSum [^double weighting, ^long max-coverage, ^long max-overlap, ^long additional-overlap]

  IObjectiveFunction

  (calculate [this, covered-set, overlap-count, candidate-set]
    (let [overlap-count (+ overlap-count additional-overlap),
          cover-count (count candidate-set),
          total-coverage (count (im/union covered-set candidate-set)),
          rel-coverage (score/relative-coverage max-coverage, total-coverage),
          rel-overlap (score/relative-overlap max-overlap, total-coverage, (+ overlap-count cover-count))]
      (+ (* weighting rel-coverage) (* (- 1.0 weighting) (- 1.0 rel-overlap)))))

  (create [this, weighting, max-coverage, max-overlap, additional-overlap]
    (WeightedMaxCoverageMinOverlapSum. weighting, max-coverage, max-overlap, additional-overlap))

  (weighting [this]
    weighting))


(deftype MaxCoverage [^long max-coverage]

  IObjectiveFunction

  (calculate [this, covered-set, overlap-count, candidate-set]
    (let [total-coverage (count (im/union covered-set candidate-set))]
      (score/relative-coverage max-coverage, total-coverage)))

  (create [this, weighting, max-coverage, max-overlap, additional-overlap]
    (MaxCoverage. max-coverage))

  (weighting [this]
    Double/NaN))


(defn weighted-max-coverage-min-overlap-sum
  [^double weighting, ^long max-coverage, ^long max-overlap, ^long additional-overlap]
  (if (zero? max-overlap)
    (MaxCoverage. max-coverage)
    (WeightedMaxCoverageMinOverlapSum. weighting, max-coverage, max-overlap, additional-overlap)))


(deftype WeightedMaxCoverageMaxOverlapSum [^double weighting, ^long max-coverage, ^long max-overlap, ^long additional-overlap]

  IObjectiveFunction

  (calculate [this, covered-set, overlap-count, candidate-set]
    (let [overlap-count (+ overlap-count additional-overlap),
          cover-count (count candidate-set),
          total-coverage (count (im/union covered-set candidate-set)),
          rel-coverage (score/relative-coverage max-coverage, total-coverage),
          rel-overlap (score/relative-overlap max-overlap, total-coverage, (+ overlap-count cover-count))]
      (+ (* weighting rel-coverage) (* (- 1.0 weighting) rel-overlap))))

  (create [this, weighting, max-coverage, max-overlap, additional-overlap]
    (WeightedMaxCoverageMaxOverlapSum. weighting, max-coverage, max-overlap, additional-overlap))

  (weighting [this]
    weighting))


(defn weighted-max-coverage-max-overlap-sum
  [^double weighting, ^long max-coverage, ^long max-overlap, ^long additional-overlap]
  (if (zero? max-overlap)
    (MaxCoverage. max-coverage)
    (WeightedMaxCoverageMaxOverlapSum. weighting, max-coverage, max-overlap, additional-overlap)))


(defn create-objective-fn
  "Create an objective function based on the previous objective function."
  [^IObjectiveFunction objective-fn, candidates]
  (let [candidates (u/->vector candidates)
        max-coverage (score/max-coverage :alterations, candidates),
        ; this function is only called for resets
        max-overlap (+
                      (score/overlap max-coverage, (score/cover-count-sum :alterations, candidates))
                      max-coverage),]
    (.create objective-fn (.weighting objective-fn), max-coverage, max-overlap, max-coverage)))


(defn new-objective-fn
  "Create a new objective function."
  [overlap-objective, weighting, candidates]
  (let [candidates (u/->vector candidates)
        max-coverage (score/max-coverage :alterations, candidates),
        max-overlap (score/overlap max-coverage, (score/cover-count-sum :alterations, candidates)),]
    (case overlap-objective
      :minimize (weighted-max-coverage-min-overlap-sum weighting, max-coverage, max-overlap, 0)
      :maximize (weighted-max-coverage-max-overlap-sum weighting, max-coverage, max-overlap, 0))))


(defn count-overlaps
  ^long [candidates]
  (let [candidates (u/->vector candidates),
        n (count candidates)]
    (loop [i 0, overlap-count 0]
      (if (< i n)
        (let [cover-set (:alterations (nth candidates i))]
          (recur
            (unchecked-inc i),
            (unchecked-add overlap-count, (count cover-set))))
        overlap-count))))


(defn determine-cover-set
  [candidates]
  (if (seq candidates)
    (reduce #(im/union %1 (:alterations %2)) (im/dense-int-set) candidates)
    (im/dense-int-set)))


(defn compare-by-score
  [^double score, ^double best-score, candidate-set, best-candidate-set]
  (if (== score, best-score)
    (> (count candidate-set) (count best-candidate-set))
    (> score best-score)))


(defn multiple-group-elements?
  [group->gene-map, group]
  (->> group (get group->gene-map) count (< 1)))


(defn find-best-pair
  [objective-fn, group->gene-map, candidate-list]
  (let [candidate-list (u/->vector candidate-list),
        n (count candidate-list)
        n-1 (unchecked-dec n)]
    ;(println "find-best-pair")
    (loop [i 0, best-candidate-1 nil, best-candidate-2 nil, best-cover-set nil, best-score 0.0]
      ;(println "best:" (:gene best-candidate-1) (:gene best-candidate-2) best-score)
      (if (< i n-1)
        (let [candidate-1 (nth candidate-list i),
              cover-set-1 (:alterations candidate-1),
              overlap-count (count cover-set-1),
              group-1 (:group candidate-1),
              allowed-candidate? (if (and group->gene-map group-1 (multiple-group-elements? group->gene-map, group-1))
                                   (fn [candidate-2]
                                     (let [group-2 (:group candidate-2)]
                                       ; second candidate must have the same group
                                       (= group-1 group-2)))
                                   (constantly true))
              [candidate-2, score] (loop [j (if group->gene-map 0 (unchecked-inc i)), best-candidate-2 nil, best-cover-set nil, best-score 0.0]
                                     (if (< j n)
                                       (let [candidate-2 (nth candidate-list j)]
                                         (if (and (not= i j) (allowed-candidate? candidate-2))
                                           (let [cover-set-2 (:alterations candidate-2),
                                                 score (calculate objective-fn, cover-set-1, overlap-count, cover-set-2)]
                                             ;(println "   " (:gene candidate-1) (:gene candidate-2) score)
                                             (if (compare-by-score score, best-score, cover-set-2, best-cover-set)
                                               (recur (unchecked-inc j), candidate-2, cover-set-2, score)
                                               (recur (unchecked-inc j), best-candidate-2, best-cover-set, best-score)))
                                           (recur (unchecked-inc j), best-candidate-2, best-cover-set, best-score)))
                                       [best-candidate-2, best-score])),
              score (double score),
              cover-set (c/union cover-set-1, (:alterations candidate-2))]
          ;(println "outer:" (:gene candidate-1) (:gene candidate-2) score)
          (if (compare-by-score score, best-score, cover-set, best-cover-set)
            (recur (unchecked-inc i), candidate-1, candidate-2, cover-set, score)
            (recur (unchecked-inc i), best-candidate-1, best-candidate-2, best-cover-set best-score)))
        ; return best pair
        (when (and best-candidate-1 best-candidate-2)
          (if (or
                ; do not modify order in group mode, if the second candidate has more than one group member
                (and group->gene-map (multiple-group-elements? group->gene-map, (:group best-candidate-2)))
                (> (count (:alterations best-candidate-1)) (count (:alterations best-candidate-2))))
            [best-candidate-1, best-candidate-2]
            [best-candidate-2, best-candidate-1]))))))


(defn find-best-candidate
  [objective-fn, covered-set, ^long overlap-count, candidate-set]
  (let [candidates-vec (vec candidate-set),
        n (count candidates-vec)]
    (loop [i 0, best-candidate nil, best-cover-set nil, best-score -1.0]
      ;(println "    current best:" (:gene best-candidate) best-score)
      (if (< i n)
        (let [candidate (nth candidates-vec i),
              cover-set (:alterations candidate),
              score (calculate objective-fn covered-set, overlap-count, cover-set)]
          ;(println (format "    best score: %.3f - %s score: %.3f" best-score (:gene candidate) score))
          (if (compare-by-score score, best-score, cover-set, best-cover-set)
            (recur (unchecked-inc i), candidate, cover-set, score)
            (recur (unchecked-inc i), best-candidate, best-cover-set, best-score)))
        best-candidate))))


(defn greedy-sort-algorithm
  [objective-fn, to-cover-set, covered-set, overlap-count, candidate-set]
  ;(println "   start greedy-sort")
  (loop [candidate-set candidate-set,
         to-cover-set to-cover-set,
         covered-set covered-set,
         overlap-count (long overlap-count),
         chosen-candidates []]
    ;(println "   #chosen =" (count chosen-candidates) "   #to-cover =" (count to-cover-set) "  #candidates =" (count candidate-set))
    (if (pos? (count candidate-set))
      (let [best-candidate (find-best-candidate objective-fn, covered-set, overlap-count, candidate-set),
            chosen-candidates (conj chosen-candidates best-candidate),
            candidate-set (disj candidate-set best-candidate),
            cover-set (:alterations best-candidate),
            covered-set (im/union covered-set cover-set),
            to-cover-set (im/difference to-cover-set cover-set),
            overlap-count (unchecked-add overlap-count (count cover-set))]
        ;(println "   chosen best:" (:gene best-candidate))
        (if (pos? (count to-cover-set))
          (recur candidate-set, to-cover-set, covered-set, overlap-count, chosen-candidates)
          ; nothing left to cover -> stop (calling algorithm may initiate a sort for remaining candidates)
          {:chosen-candidates chosen-candidates,
           :candidate-set candidate-set}))
      {:chosen-candidates chosen-candidates
       :candidate-set nil})))


(defn candidates-by-group
  [candidates]
  (persistent!
    (reduce
      (fn [result-map, {:keys [group] :as cand}]
        (cond-> result-map
          group
          (assoc! group (-> (get result-map group #{}) (conj cand)))))
      (transient {})
      candidates)))


(defn resetting-greedy-sort-algorithm
  "Runs the greedy-sort-algorithm at least once and iteratively for the remaining candidates.
  Each iteration starts with a new to-cover-set of the remaining candidates, the objective function is reinitialized
  and the other state is initialized."
  [objective-fn, to-cover-set, covered-set, overlap-count, candidate-set]
  (if (empty? candidate-set)
    ; no candidates => cannot chose anything
    {:chosen-candidates nil,
     :candidate-set candidate-set,
     :objective-fn objective-fn}
    ; candidates => run the greedy sort at least once
    (let [{:keys [candidate-set, chosen-candidates]} (if (pos? (count to-cover-set))
                                                       ; sort
                                                       (greedy-sort-algorithm objective-fn, to-cover-set, covered-set, overlap-count, candidate-set)
                                                       ; restart because nothing left to cover
                                                       {:chosen-candidates nil, :candidate-set candidate-set})]
      (if (empty? candidate-set)
        ; no candidates left => return chosen candidates
        {:chosen-candidates chosen-candidates,
         :candidate-set nil,
         :objective-fn objective-fn}
        ; candidates left => iteratively run greedy sort
        (loop [candidate-set candidate-set,
               objective-fn objective-fn,
               total-chosen-candidates (transient (vec chosen-candidates))]
          (if (seq candidate-set)
            ; candidates left => run greedy sort again
            (let [to-cover-set (determine-cover-set candidate-set)]
              (if (seq to-cover-set)
                (let [objective-fn (create-objective-fn objective-fn, candidate-set),
                      {:keys [candidate-set, chosen-candidates]} (greedy-sort-algorithm
                                                                   objective-fn,
                                                                   to-cover-set,
                                                                   (im/dense-int-set),
                                                                   0,
                                                                   candidate-set),
                      total-chosen-candidates (u/into! total-chosen-candidates chosen-candidates)]
                  (recur candidate-set, objective-fn, total-chosen-candidates))
                ; the remaining candidates do not cover any samples
                {:chosen-candidates (persistent!
                                      (u/into! total-chosen-candidates
                                        (sort-by (some-fn :gene :group) candidate-set)))
                 :candidate-set nil,
                 :objective-fn objective-fn}))
            ; no candidates left => return chosen candidates
            {:chosen-candidates (persistent! total-chosen-candidates),
             :candidate-set nil,
             :objective-fn objective-fn}))))))


(defn ensure-complete-group
  [objective-fn, to-cover-set, covered-set, group->gene-map, chosen-candidates]
  (let [groups (->> chosen-candidates (keep :group) distinct (filterv #(multiple-group-elements? group->gene-map %)))
        overlap-count (count-overlaps chosen-candidates)]
    (assert (#{0, 1} (count groups)) "")
    (if-let [group (first groups)]
      (let [group-members (get group->gene-map group),
            group-candidate-set (reduce disj group-members chosen-candidates),
            {sorted-group-members :chosen-candidates, :keys [objective-fn]} (resetting-greedy-sort-algorithm objective-fn, to-cover-set, covered-set, overlap-count, group-candidate-set),
            completed-group (into chosen-candidates sorted-group-members)]
        {:chosen-candidates completed-group,
         :overlap-count (count-overlaps completed-group),
         :objective-fn objective-fn})
      {:chosen-candidates chosen-candidates,
       :overlap-count overlap-count,
       :objective-fn objective-fn})))


(defn remove-covered-samples
  [sample-set, gene-list]
  (reduce
    (fn [sample-set, gene-data]
      (im/difference sample-set, (:alterations gene-data)))
    sample-set
    gene-list))


(defn setup-with-ensured-preselection
  "
  Assumes that candidate-set and preselected-candidates are disjoint."
  [objective-fn, candidate-set, preselected-candidates, group?]
  (let [non-empty-preselected-candidates? (boolean (some (comp pos? count :alterations) preselected-candidates)),
        to-cover-set (remove-covered-samples (determine-cover-set candidate-set) preselected-candidates)]
    (if non-empty-preselected-candidates?
      {:preselection (vec preselected-candidates),
       :objective-fn objective-fn,
       :overlap-count (count-overlaps preselected-candidates),
       :to-cover-set to-cover-set,
       :covered-set (determine-cover-set preselected-candidates),
       :candidate-set candidate-set}
      ; need to find the best pair because no preselected candidates with alterations found
      (let [group->gene-map (when group? (candidates-by-group candidate-set)),
            best-pair (find-best-pair objective-fn, group->gene-map, candidate-set)]
        (if group?
          (let [covered-set (determine-cover-set best-pair),
                ; all group members are returned in chosen-candidates
                {:keys [chosen-candidates, overlap-count, objective-fn]} (ensure-complete-group objective-fn, to-cover-set, covered-set, group->gene-map, best-pair)]
            {:preselection chosen-candidates,
             :objective-fn objective-fn,
             :overlap-count overlap-count,
             :to-cover-set (remove-covered-samples to-cover-set, chosen-candidates),
             :covered-set (determine-cover-set chosen-candidates),
             :candidate-set (reduce disj candidate-set chosen-candidates)})
          (let [preselection (into (vec preselected-candidates) best-pair)]
            {:preselection preselection,
             :objective-fn objective-fn,
             :overlap-count (count-overlaps preselection),
             :to-cover-set (remove-covered-samples to-cover-set, best-pair),
             :covered-set (determine-cover-set best-pair),
             :candidate-set (reduce disj candidate-set best-pair)}))))))


(defn greedy-group-sort-algorithm
  [objective-fn, to-cover-set, covered-set, overlap-count, candidate-set]
  (let [group->gene-map (candidates-by-group candidate-set)]
    (loop [candidate-set candidate-set,
           objective-fn objective-fn,
           to-cover-set to-cover-set,
           covered-set covered-set,
           total-overlap-count overlap-count,
           total-chosen-candidates []]
      ;(println
      ;  "#chosen =" (count total-chosen-candidates) "  #candidates =" (count candidate-set)
      ;  "  #covered =" (count covered-set) "  #to-cover =" (count to-cover-set))
      ;(print "chosen: ")
      ;(u/print-gene-names total-chosen-candidates)
      ;(print "candidates: ")
      ;(u/print-gene-names candidate-set)
      (if (seq candidate-set)
        (if (pos? (count to-cover-set))
          (let [best-candidate (find-best-candidate objective-fn, covered-set, total-overlap-count, candidate-set),
                best-candidate-group (:group best-candidate)]
            ;(println "best =" (:gene best-candidate) "  group =" best-candidate-group)
            (if (and best-candidate-group (multiple-group-elements? group->gene-map, best-candidate-group))
              ; add other group members
              (let [group-members (-> (get group->gene-map best-candidate-group) set (disj best-candidate)),
                    ; only use group members that are still considered as candidates
                    group-members (persistent!
                                    (reduce
                                      (fn [members, gene]
                                        (cond-> members (contains? candidate-set gene) (conj! gene)))
                                      (transient #{})
                                      group-members)),
                    {greedy-chosen-candidates :chosen-candidates, :keys [objective-fn]} (resetting-greedy-sort-algorithm objective-fn, to-cover-set, covered-set, total-overlap-count, group-members),
                    chosen-candidates (into [best-candidate] greedy-chosen-candidates),
                    candidate-set (reduce disj candidate-set chosen-candidates),
                    additionally-covered (determine-cover-set chosen-candidates),
                    to-cover-set (im/difference to-cover-set, additionally-covered),
                    covered-set (im/union covered-set, additionally-covered),
                    overlap-count (count-overlaps chosen-candidates),
                    total-overlap-count (unchecked-add total-overlap-count overlap-count),
                    total-chosen-candidates (into total-chosen-candidates chosen-candidates)]
                (recur
                  candidate-set,
                  objective-fn,
                  to-cover-set,
                  covered-set,
                  total-overlap-count,
                  total-chosen-candidates))
              ; no group or no group members left, continue ...
              (let [candidate-set (disj candidate-set best-candidate),
                    additionally-covered (:alterations best-candidate),
                    to-cover-set (im/difference to-cover-set, additionally-covered),
                    covered-set (im/union covered-set, additionally-covered),
                    overlap-count (count additionally-covered),
                    total-overlap-count (unchecked-add total-overlap-count overlap-count)
                    total-chosen-candidates (conj total-chosen-candidates best-candidate)]
                (recur
                  candidate-set,
                  objective-fn,
                  to-cover-set,
                  covered-set,
                  total-overlap-count,
                  total-chosen-candidates))))
          ; perform reset
          (let [objective-fn (create-objective-fn objective-fn, candidate-set), ; build new objective function relative to remaining candidate set
                to-cover-set (determine-cover-set candidate-set),
                covered-set (im/dense-int-set),
                overlap-count 0]
            ;(println "RESET #to-cover =" (count to-cover-set))
            (recur
              candidate-set,
              objective-fn,
              to-cover-set,
              covered-set,
              overlap-count,
              total-chosen-candidates)))
        total-chosen-candidates))))


(defn greedy-sort
  [{:keys [weighting, overlap-objective, group?, preselected-candidates] :as settings}, candidate-list]
  (if (< (count candidate-list) 2)
    ; no need to sort since there are less than 2 candidates
    (cond->> candidate-list (seq preselected-candidates) (into (vec preselected-candidates)))
    ; sort
    (let [; choose objective function based on optimization type for overlap
          objective-fn (new-objective-fn overlap-objective, weighting, candidate-list),
          ; only use group mode if there is group with at least 2 candidates
          group? (and group? (->> candidate-list (keep :group) frequencies vals (some #(> % 1)) boolean)),
          ; make candidate-set and preselected-candidates disjoint
          candidate-set (reduce disj (set candidate-list) preselected-candidates),
          {:keys [preselection,
                  objective-fn,
                  overlap-count,
                  to-cover-set,
                  covered-set,
                  candidate-set]} (setup-with-ensured-preselection objective-fn, candidate-set, preselected-candidates, group?)]
      (into (vec preselection)
        (if group?
          (greedy-group-sort-algorithm objective-fn, to-cover-set, covered-set, overlap-count, candidate-set)
          (:chosen-candidates (resetting-greedy-sort-algorithm objective-fn, to-cover-set, covered-set, overlap-count, candidate-set)))))))