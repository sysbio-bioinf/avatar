; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.mo-select
  (:require
    [avatar.algorithms.score :as score]
    [avatar.algorithms.common :as c]
    [avatar.algorithms.greedy-setcover :as gs]
    [avatar.util :as u]
    [clojure.data.int-map :as im]
    [com.climate.claypoole :as cp]
    [avatar.algorithms.progress-tracker :as progress]
    [clojure.tools.logging :as log]
    [clojure.pprint :as pp]
    [clojure.set :as set]
    [avatar.data.gene-data :as gd])
  (:import
    (org.uma.jmetal.solution BinarySolution Solution)
    (org.uma.jmetal.operator.impl.crossover SinglePointCrossover)
    (org.uma.jmetal.operator.impl.mutation BitFlipMutation)
    (org.uma.jmetal.operator.impl.selection TournamentSelection)
    (org.uma.jmetal.algorithm.multiobjective.nsgaii NSGAIIBuilder NSGAIIBuilder$NSGAIIVariant)
    (org.uma.jmetal.algorithm.multiobjective.spea2 SPEA2Builder)
    (org.uma.jmetal.util SolutionListUtils)
    (org.uma.jmetal.util.evaluator SolutionListEvaluator)
    (org.uma.jmetal.algorithm Algorithm)
    (org.uma.jmetal.util.binarySet BinarySet)
    (org.uma.jmetal.algorithm.multiobjective.gwasfga GWASFGA)
    (org.uma.jmetal.util.comparator RankingAndCrowdingDistanceComparator)
    (org.uma.jmetal.qualityindicator.impl.hypervolume PISAHypervolume)
    (org.uma.jmetal.util.front.imp ArrayFront)
    (org.uma.jmetal.problem BinaryProblem Problem)
    (org.uma.jmetal.solution.impl DefaultBinarySolution)
    (org.uma.jmetal.util.pseudorandom JMetalRandom)
    (java.lang.reflect Array)
    (clojure.lang IFn$OLL)
    (java.util List)))



(defn inc-int-array-entry
  ^ints [^ints a, ^long pos]
  (let [pos (unchecked-int pos),
        v (Array/getInt a, pos)]
    (Array/setInt a, pos, (unchecked-inc-int v))
    a))


(defn dec-int-array-entry
  ^ints [^ints a, ^long pos]
  (let [pos (unchecked-int pos),
        v (Array/getInt a, pos)]
    (Array/setInt a, pos, (unchecked-dec-int v))
    a))


(defn add-sample-covers
  [sample-cover-count-array, sample-set]
  (reduce
    (fn [^ints cover-count-array, ^long sample]
      (inc-int-array-entry cover-count-array, sample))
    sample-cover-count-array
    sample-set))


(defn count-covering-genes
  ^ints [^long sample-array-size, ^BinarySet selected-genes, sample-set-vector]
  (loop [pos (.nextSetBit selected-genes 0), sample-cover-count-array (make-array Integer/TYPE sample-array-size)]
    (if (neg? pos)
      ; done iterating through all selected genes => return array
      sample-cover-count-array
      (let [sample-set (nth sample-set-vector pos)
            cover-count-array (add-sample-covers sample-cover-count-array, sample-set)]
        (recur (.nextSetBit selected-genes (unchecked-inc pos)), cover-count-array)))))


(defn redundant-gene?
  [^ints sample-cover-count-array, sample-set]
  (reduce
    (fn [redundant?, sample]
      (if (< (aget sample-cover-count-array sample) 2)
        ; not redundant, at least one sample is covered less than twice (short circuiting via reduced)
        (reduced false)
        redundant?))
    true
    sample-set))


(defn delete-sample-covers
  [sample-cover-count-array, sample-set]
  (reduce
    (fn [^ints sample-cover-count-array, sample]
      (dec-int-array-entry sample-cover-count-array, sample))
    sample-cover-count-array
    sample-set))


(defn remove-redundant-genes
  [^long sample-array-size, forward?, sample-set-vector, ^BinarySet selected-genes]
  "Remove redundant genes (i.e. genes that do not cover additional samples)
  by checking from genes with most covered samples to genes with least covered samples when forward? = true
  and vice versa for forward? = false."
  (let [sample-cover-count-array (count-covering-genes sample-array-size, selected-genes, sample-set-vector)
        ^IFn$OLL next-set-bit (if forward?
                                (fn forward ^long [^BinarySet bitset, ^long current-index]
                                  (.nextSetBit bitset (inc current-index)))
                                (fn backward ^long [^BinarySet bitset, ^long current-index]
                                  (.previousSetBit bitset (dec current-index)))),
        first-set-bit (if forward?
                        (.nextSetBit selected-genes 0)
                        (.previousSetBit selected-genes (dec (.size selected-genes))))]
    ; genes are sorted by sample set size decreasingly (redundant genes with larger sample sets are removed first on forward? = true and last on forward? = false)
    (loop [pos first-set-bit, sample-cover-count-array sample-cover-count-array, selected-genes selected-genes]
      (if (neg? pos)
        ; done iterating through all selected genes => return potentially altered selected-genes
        selected-genes
        ; detect whether current gene is redundant
        (let [sample-set (nth sample-set-vector pos)]
          (if (redundant-gene? sample-cover-count-array, sample-set)
            ; remove gene
            (recur
              (.invokePrim next-set-bit selected-genes pos),
              (delete-sample-covers sample-cover-count-array, sample-set),
              ; remove gene
              (doto selected-genes
                (.clear pos)))
            ; keep gene
            (recur
              (.invokePrim next-set-bit selected-genes pos),
              sample-cover-count-array,
              selected-genes)))))))



(definterface IEvaluation
  (evaluate [solution]))


(defn median-sample-count
  [sample-set-vector ^BinarySet selected-genes]
  (let [vals (loop [pos (.nextSetBit selected-genes 0), vals (transient [])]
               (if (neg? pos)
                 (persistent! vals)
                 (recur
                   (.nextSetBit selected-genes (unchecked-inc pos))
                   (conj! vals (count (nth sample-set-vector pos))))))
        n (count vals)
        vals (vec (sort vals))]
    (if (even? n)
      (/ (+ (nth vals (quot n 2)) (nth vals (dec (quot n 2)))) 2.0)
      (nth vals (quot n 2)))))


(deftype EvaluationMeasures [^long covered-count, ^long overlap-count, ^long gene-count])


(defn evaluate-gene-selection
  [sample-set-vector, ^BinarySet gene-set]
  (loop [pos (.nextSetBit gene-set 0), covered (im/dense-int-set), overlap-count 0, gene-count 0]
    (if (neg? pos)
      ; calculate total result
      (let [covered-count (count covered)]
        (EvaluationMeasures. covered-count, overlap-count, gene-count))
      ; get sample set for gene at position `pos`
      (let [sample-set (nth sample-set-vector pos)]
        (recur
          ; next set bit
          (.nextSetBit gene-set (unchecked-inc pos)),
          ; update covered set
          (im/union covered, sample-set),
          ; update overlap
          (unchecked-add overlap-count (count sample-set))
          ; update gene count
          (unchecked-inc gene-count))))))


(deftype Evaluation [^long sample-array-size, sample-set-vector, objectives-fn]
  IEvaluation
  (evaluate [_, solution]
    (let [^BinarySet selected-genes (.getVariableValue ^BinarySolution solution 0),
          remove-redundancy+evaluate (fn remove-redundancy+evaluate [forward?, ^BinarySet gene-set]
                                       (let [gene-set (->> (.clone gene-set)
                                                        (remove-redundant-genes sample-array-size, forward?, sample-set-vector))]
                                         {:gene-set gene-set,
                                          :objectives (objectives-fn (evaluate-gene-selection sample-set-vector, gene-set))}))
          {forward-set :gene-set, forward-objectives :objectives} (remove-redundancy+evaluate true, selected-genes),
          {backward-set :gene-set, backward-objectives :objectives} (remove-redundancy+evaluate false, selected-genes)
          [gene-set, objectives] (if (< (second forward-objectives) (second backward-objectives))
                                   [forward-set, forward-objectives]
                                   [backward-set, backward-objectives])]
      (u/reduce-indexed
        (fn [solution, index, objective-value]
          (doto ^BinarySolution solution
            (.setObjective index, objective-value)))
        (doto ^BinarySolution solution
          (.setVariableValue 0, gene-set))
        objectives))))


(defn coverage-overlap-objective
  [gene-list]
  (let [sample-array-size (c/determine-max-sample-count gene-list),
        max-coverage (double (score/max-coverage :alterations, gene-list)),
        max-overlap (double (score/overlap max-coverage, (score/cover-count-sum :alterations, gene-list)))]
    (Evaluation. sample-array-size, (mapv :alterations gene-list),
      (fn [^EvaluationMeasures measures]
        ; jMetal tries to minimize all objectives
        [; coverage
         (- 1.0 (/ (.covered-count measures) max-coverage))
         ; overlap
         (/ (unchecked-subtract (.overlap-count measures) (.covered-count measures)) max-overlap)]))))


(defn coverage-gene-count-objective
  [gene-list]
  (let [sample-array-size (c/determine-max-sample-count gene-list),
        max-coverage (double (score/max-coverage :alterations, gene-list))
        max-gene-count (double (count gene-list))]
    (Evaluation. sample-array-size, (mapv :alterations gene-list),
      (fn [^EvaluationMeasures measures]
        ; jMetal tries to minimize all objectives
        [; coverage
         (- 1.0 (/ (.covered-count measures) max-coverage))
         ; gene count
         (/ (.gene-count measures) max-gene-count)]))))


(defn create-random-solution
  [^BinaryProblem problem, ^double selection-probability, ^long n]
  (let [solution (DefaultBinarySolution. problem)
        ^BinarySet variable (.getVariableValue solution, 0)
        prng (JMetalRandom/getInstance)]
    (loop [i 0]
      (when (< i n)
        (.set variable i (< (.nextDouble prng) selection-probability))
        (recur (unchecked-inc i))))
    solution))


(defn create-selection-problem
  [{:keys [selection-probability, second-objective] :as parameter-map}, gene-list]
  (let [n (count gene-list),
        selection-probability (double (or selection-probability 0.5)),
        evaluation-fn (case second-objective
                        :overlap (coverage-overlap-objective gene-list)
                        :gene-count (coverage-gene-count-objective gene-list))]
    (reify BinaryProblem

      (getNumberOfVariables [_]
        1)

      (getNumberOfObjectives [_]
        2
        ; TEST ONLY:
        #_3)

      (getNumberOfConstraints [_]
        0)

      (getName [_]
        "Gene Selection Problem")

      (createSolution [this]
        (create-random-solution this, selection-probability, n))

      (getNumberOfBits [_, index]
        (when-not (== index 0)
          (u/illegal-argument "This problem has only a single variable! (Index = %s)" index))
        n)

      (getTotalNumberOfBits [_] n)

      (evaluate [_, solution]
        (.evaluate ^IEvaluation evaluation-fn solution)))))



(defn extract-selected-genes
  [entity-list, ^BinarySolution solution]
  (let [^BinarySet selected-bits (.getVariableValue solution 0)]
    (loop [pos (.nextSetBit selected-bits 0), selection (transient [])]
      (if (neg? pos)
        (persistent! selection)
        (recur
          (.nextSetBit selected-bits (unchecked-inc pos)),
          (conj! selection (nth entity-list pos)))))))


(defn partition-into-parts
  [^long part-count, coll]
  (let [v (vec coll),
        n (count coll)
        part-size (quot n part-count),
        larger-parts (mod n part-count)]
    (loop [part-index 0, start-offset 0, part-list (transient [])]
      (if (< part-index part-count)
        (let [part-size (cond-> part-size (< part-index larger-parts) unchecked-inc),
              end-offset (+ start-offset part-size),
              part (subvec v, start-offset, end-offset)]
          (recur
            (unchecked-inc part-index),
            end-offset,
            (conj! part-list part)))
        (persistent! part-list)))))


(defn solution-list-evaluator
  ^SolutionListEvaluator [thread-count, progress-tracker]
  (let [thread-count (or thread-count 1)
        evaluate (fn [^Problem problem, ^Solution solution]
                   (.evaluate problem solution))
        report-progress (if progress-tracker
                          (fn [^List solution-list]
                            (progress/update progress-tracker, (.size solution-list)))
                          (fn [_]
                            #_nothing_to_do))]
    (if (> thread-count 1)
      (let [pool (cp/threadpool thread-count)]
        (reify SolutionListEvaluator
          (evaluate [_, solution-list, problem]
            (let [partitions (partition-into-parts thread-count, solution-list)]
              (doall
                (cp/pmap pool,
                  (fn [solution-list-partition]
                    (u/for-each-indexed!
                      (fn [_, solution]
                        (evaluate problem, solution))
                      solution-list-partition)),
                  partitions)))
            (report-progress solution-list)
            solution-list)
          (shutdown [_] (cp/shutdown pool))))
      ; single threaded
      (reify SolutionListEvaluator
        (evaluate [_, solution-list, problem]
          (u/for-each-indexed!
            (fn [_, solution]
              (evaluate problem, solution))
            solution-list)
          (report-progress solution-list)
          solution-list)
        (shutdown [_] #_nothing-to-do)))))


(defn create-algorithm
  [problem, {:keys [algorithm, crossover-probability, mutation-probability, tournament-size, iteration-count, population-size, thread-count, seed, progress-tracker]}]
  ; HUXCrossover causes strange convergence to single individual populations
  (let [crossover (SinglePointCrossover. (or crossover-probability 0.9)),
        mutation (BitFlipMutation. (or mutation-probability 0.2)),
        ; proper NSGA-II uses BinaryTournament and RankingAndCrowdingDistanceComparator
        selection (TournamentSelection. (RankingAndCrowdingDistanceComparator.), (or tournament-size 2)),
        population-size (or population-size 100),
        iteration-count (or iteration-count 100),
        max-evaluations (* population-size iteration-count),
        evaluator (solution-list-evaluator thread-count, progress-tracker)]
    ; if there is a given seed, initialize the random singleton
    (when seed
      (.setSeed (JMetalRandom/getInstance) (long seed)))
    {:algorithm (case (or algorithm :NSGAII)
                  :NSGAII (-> (NSGAIIBuilder. problem, crossover, mutation)
                            (.setMaxEvaluations max-evaluations)
                            (.setPopulationSize population-size)
                            (.setSelectionOperator selection)
                            (.setSolutionListEvaluator evaluator)
                            (.setVariant NSGAIIBuilder$NSGAIIVariant/NSGAII)
                            .build)
                  :SteadyStateNSGAII (-> (NSGAIIBuilder. problem, crossover, mutation)
                                       (.setMaxEvaluations max-evaluations)
                                       (.setPopulationSize population-size)
                                       (.setSelectionOperator selection)
                                       (.setSolutionListEvaluator evaluator)
                                       (.setVariant NSGAIIBuilder$NSGAIIVariant/SteadyStateNSGAII)
                                       .build)
                  ; NSGA-III behaves differently to the other algorithms here. Does not seem to adhere to iteration-count and population-size (jMetal 5.3)
                  ;:NSGAIII (-> (NSGAIIIBuilder. problem)
                  ;           (.setMaxIterations iteration-count)
                  ;           (.setPopulationSize population-size)
                  ;           (.setCrossoverOperator crossover)
                  ;           (.setMutationOperator mutation)
                  ;           (.setSelectionOperator selection)
                  ;           (.setSolutionListEvaluator evaluator)
                  ;           .build)
                  :SPEA2 (-> (SPEA2Builder. problem, crossover, mutation)
                           (.setMaxIterations iteration-count)
                           (.setPopulationSize population-size)
                           (.setSelectionOperator selection)
                           (.setSolutionListEvaluator evaluator)
                           .build)
                  :GWASFGA (GWASFGA. problem, population-size, iteration-count, crossover, mutation, selection, evaluator))
     :evaluator evaluator}))


(defn transform-pareto-front
  [second-objective, gene-id->gene-map, sample-count, input-gene-list, pareto-front]
  (let [sample-count (double sample-count)]
    (vec
      (sort-by
        (juxt :coverage second-objective)
        (remove
          (fn [{:keys [gene-count]}]
            (zero? gene-count))
          (mapv
            (fn [solution]
              (let [input-selection (extract-selected-genes input-gene-list, solution),
                    result-selection (mapv (fn [{:keys [gene-id]}] (get gene-id->gene-map gene-id)) input-selection),
                    ; in case the selection is only performed for a single sample group
                    ; calculate the objectives only for this group (input-selection from input-gene-list)
                    coverage (score/total-coverage :alterations, input-selection),
                    overlap (score/total-overlap :alterations, input-selection)]
                {:selection result-selection,
                 :gene-count (count result-selection),
                 :coverage coverage,
                 :overlap overlap,
                 ; for less confusion, use relative coverage and mean overlap
                 ; (in contrast to previously objectives relative to maximal number of coverable samples)
                 :relative-coverage (/ coverage sample-count),
                 :relative-overlap (/ overlap sample-count)}))
            pareto-front))))))


(defn hypervolume
  [solution-list]
  (-> (PISAHypervolume. (ArrayFront. 1, 2))
    (.evaluate solution-list)))


(defn adjusted-hypervolume
  "Renormalize solution objectives relative to a greedy solution."
  [second-objective, gene-list, solution-list]
  (let [greedy-solution (gs/greedy-setcover gene-list),
        max-coverage (double (score/total-coverage :alterations, greedy-solution)),
        second-objective-fn (case second-objective
                              :overlap (fn [selected-genes] (score/total-overlap :alterations, selected-genes)),
                              :gene-count (fn [selected-genes] (count selected-genes))),
        max-second-objective (double (second-objective-fn greedy-solution)),
        renormalized-solution-list (mapv
                                     (fn [^BinarySolution solution]
                                       (let [selected-genes (extract-selected-genes gene-list, solution)
                                             coverage (score/total-coverage :alterations, selected-genes),
                                             second-objective (second-objective-fn selected-genes)]
                                         (doto (.copy solution)
                                           (.setObjective 0, (- 1.0 (/ coverage max-coverage)))
                                           (.setObjective 1, (/ second-objective max-second-objective)))))
                                     (vec solution-list))]
    (hypervolume renormalized-solution-list)))



(defn sort-by-coverage
  [gene-list]
  (->> gene-list
    (map
      (fn [index, gene]
        (assoc gene
          ::index index
          ::coverage (-> gene :alterations count)))
      (range))
    (sort-by ::coverage >)
    vec))


(defn multi-objective-selection
  [{:keys [select-only-for-sample-group, minimum-alteration-ratio, alteration-type, second-objective, seed] :as parameter-map}, sample-group-map, gene-list]
  (when seed
    (.setSeed (JMetalRandom/getInstance) (long seed)))
  (let [gene-id->gene-map (zipmap (mapv :gene-id gene-list) gene-list),
        input-gene-list (->> gene-list
                          (c/select-alterations-in-gene-list alteration-type)
                          sort-by-coverage
                          (c/maybe-remove-sample-alterations-of-other-groups select-only-for-sample-group, sample-group-map)
                          (gd/remove-genes-with-less-alterations
                            (long
                              (Math/ceil
                                (* minimum-alteration-ratio (gd/sample-group-size sample-group-map, select-only-for-sample-group))))))]
    (if (seq input-gene-list)
      (let [sample-count (gd/sample-count-in-group select-only-for-sample-group, sample-group-map)
            problem (create-selection-problem parameter-map, input-gene-list),
            {:keys [algorithm, evaluator]} (create-algorithm problem, parameter-map),
            start-time (System/currentTimeMillis)
            _ (.run ^Algorithm algorithm),
            stop-time (System/currentTimeMillis)
            _ (.shutdown ^SolutionListEvaluator evaluator)
            pareto-front (SolutionListUtils/getNondominatedSolutions (.getResult ^Algorithm algorithm))]
        {:hypervolume (adjusted-hypervolume second-objective, input-gene-list, pareto-front)
         :pareto-front (transform-pareto-front second-objective, gene-id->gene-map, sample-count, input-gene-list, pareto-front),
         :runtime (- stop-time start-time),
         :parameters (-> parameter-map
                       (set/rename-keys {:select-only-for-sample-group :sample-group})
                       (dissoc :show-progress? :progress-tracker :thread-count))})
      {:hypervolume 0.0
       :pareto-front [],
       :runtime 0,
       :parameters (-> parameter-map
                     (set/rename-keys {:select-only-for-sample-group :sample-group})
                     (dissoc :show-progress? :progress-tracker :thread-count))})))


(defn optimize-selection
  [parameter-map, sample-group-map, gene-list]
  (log/debugf "multi-objective gene selection parameters:\n%s" (with-out-str (pp/pprint parameter-map)))
  (let [{:keys [hypervolume] :as result-map} (u/timing "multi-objective gene selection"
                                               (multi-objective-selection parameter-map, sample-group-map, gene-list))]
    (log/debugf "multi-objective gene selection: hypervolume = %s" hypervolume)
    result-map))


(defn add-fixed-genes
  [fixed-genes, pareto-front]
  (mapv
    (fn [solution]
      (update-in solution [:selection] #(into fixed-genes %)))
    pareto-front))


(defn add-plot-data
  [gene-sample-data, pareto-front]
  (mapv
    (fn [{:keys [selection] :as solution-data}]
      (assoc solution-data
        :plot-data (assoc gene-sample-data :gene-list selection)))
    pareto-front))


(defn multi-objective-gene-selection
  [{:keys [gene-list, sample-group-map] :as gene-sample-data}, {:keys [exclude-fixed-rows?] :as setup-data}]
  (let [result-map (optimize-selection
                     setup-data,
                     sample-group-map,
                     (cond-> gene-list exclude-fixed-rows? (->> (remove :order-fixed?) vec)))]
    (update-in result-map [:pareto-front]
      (fn [pareto-front]
        (cond->> (distinct pareto-front)
          exclude-fixed-rows? (add-fixed-genes (filterv :order-fixed? gene-list))
          true (add-plot-data gene-sample-data))))))