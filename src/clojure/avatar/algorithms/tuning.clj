; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.tuning
  (:require
    [frost.quick-freeze :as qf]
    [avatar.algorithms.mo-select :as mo-select]
    [clojure.tools.cli :as cli]
    [clojure.string :as str]))


(defn parse-double
  ^double [^String s]
  (Double/parseDouble s))


(defn parse-long
  ^long [^String s]
  (Long/parseLong s))


(defn probability-validation
  [probability-name]
  [#(<= 0.0 % 1.0) (format "%s must be a value in (0.0, 1.0)!" probability-name)])


(def cli-spec
  [["-a" "--algorithm A" :parse-fn keyword]
   ["-s" "--selection-probability P" :parse-fn parse-double, :validate (probability-validation "--selection-probability")]
   ["-c" "--crossover-probability P" :parse-fn parse-double, :validate (probability-validation "--crossover-probability")]
   ["-m" "--mutation-probability P" :parse-fn parse-double, :validate (probability-validation "--mutation-probability")]
   ["-t" "--tournament-size S" :parse-fn parse-long]
   ["-p" "--population-size S" :parse-fn parse-long]
   ["-b" "--evaluation-budget B" :parse-fn parse-long]
   ["-S" "--seed S" :parse-fn parse-long]])


(defn trim-cli-args
  [args]
  (mapv #(cond-> % (string? %) str/trim) args))


(defn irace-run
  [{:keys [instance, parameters]}]
  (let [gene-list (qf/quick-file-defrost instance),
        {:keys [arguments, summary, errors], parameter-map :options} (cli/parse-opts (trim-cli-args parameters), cli-spec),
        parameter-map (let [{:keys [evaluation-budget, population-size]} parameter-map]
                        (assoc parameter-map :iteration-count (quot evaluation-budget population-size))),
        {:keys [pareto-front]} (mo-select/multi-objective-selection gene-list, parameter-map)]
    (if errors
      (println errors "\n" summary)
      ; maximise hypervolume - irace minimises costs
      {:costs (- 1.0 (mo-select/hypervolume pareto-front))})))