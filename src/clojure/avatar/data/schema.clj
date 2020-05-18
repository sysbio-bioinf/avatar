; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.schema
  (:require
    [schema.core :as s]
    [avatar.util :as u])
  (:import
    (clojure.data.int_map PersistentIntSet)))



(s/defschema FloatNum
  (s/pred float? "floating point number"))


(defn alteration-type?
  [x]
  (contains?
    #{:mutation-data,
      :expression-data,
      :methylation-data}
    x))


(s/defschema AlterationType
  (s/pred alteration-type?, "alteration type"))


(s/defschema Gene
  {:gene (s/maybe s/Str), ; a gene name is required, but there is the special case when only a group is set
   :group (s/maybe s/Str),
   :order-fixed? s/Bool,
   :gene-id s/Int,
   :alteration-data {AlterationType {:alterations PersistentIntSet,
                                     :missing-values PersistentIntSet}}})


(s/defschema GeneSampleData
  {:alteration-type-set #{AlterationType},
   :clinical-attributes #{s/Str},
   :clinical-data-map {s/Int (s/maybe {s/Str (s/maybe s/Str)})},
   :gene-list [Gene],
   :sample-count s/Int
   :sample-group-map {s/Int (s/maybe s/Str)},
   :sample-id-vec [s/Str]
   :sample-permutation {:sample->column [s/Int],
                        :column->sample [s/Int]}})


(s/defschema SecondObjective
  (s/either
    (s/eq :overlap)
    (s/eq :gene-count)))


(s/defschema ParetoFrontSolution
  {:coverage s/Int,
   :overlap s/Int,
   :gene-count s/Int,
   :relative-coverage FloatNum,
   :relative-overlap FloatNum,
   :plot-data GeneSampleData,
   :selection [Gene]})


(s/defschema OptimizationParameters
  {:alteration-type AlterationType,
   :sample-group (s/maybe s/Str),
   :second-objective SecondObjective,
   :minimum-alteration-ratio s/Num,
   :algorithm s/Keyword,
   :exclude-fixed-rows? s/Bool,
   :mutation-probability s/Num,
   :crossover-probability s/Num,
   :selection-probability s/Num,
   :iteration-count s/Int,
   :tournament-size s/Int
   :population-size s/Int})


(s/defschema ParetoFront
  {:hypervolume FloatNum,
   :pareto-front [ParetoFrontSolution],
   :runtime s/Int,
   :parameters OptimizationParameters})


(s/defschema Snapshot
  {:id s/Int,
   :created s/Str,
   :name s/Str,
   :shown-alteration-type AlterationType,
   :data GeneSampleData})


(defn font-weight?
  [x]
  (contains?
    #{:extra-light :bold :semi-bold :medium :normal :thin :extra-bold :light :black}
    x))


(s/defschema FontWeight
  (s/pred font-weight? "font weight"))


(defn font-posture?
  [x]
  (contains?
    #{:regular, :italic}
    x))


(s/defschema FontPosture
  (s/pred font-posture? "font posture"))


(s/defschema Font
  {:family s/Str,
   :weight FontWeight,
   :posture FontPosture,
   :size s/Num})


(s/defschema PlotStyle
  {:even-row-color s/Str,
   :odd-row-color s/Str,
   :even-column-group-color s/Str,
   :odd-column-group-color s/Str,
   :alteration-color s/Str,
   :overlap-color s/Str,
   :missing-value-color s/Str,
   :shown-alteration-type AlterationType,
   :paint-missing-values? s/Bool,
   :border-color s/Str,
   :border-width s/Num,
   :alteration-cell-width s/Num,
   :alteration-ratio-digits s/Num,
   :fixed-row-header-color s/Str,
   :selection-color s/Str,
   :gene-group-font Font,
   :sample-group-font Font,
   :alteration-ratio-font Font,
   :min-gene-group-width s/Num,
   :min-row-height s/Num,
   :gene-name-separator-spacing s/Num,
   :gene-name-separator s/Str,
   :gene-name-left-font Font,
   :gene-name-right-font Font,
   :sample-groups-colored? s/Bool,
   :sample-group->color {(s/maybe s/Str) s/Str}})


(s/defschema NamedParetoFront
  {:name s/Str,
   :data ParetoFront})


(s/defschema BatchParetoFront
  {:name [s/Str],
   :snapshot s/Str,
   :data ParetoFront})


(s/defschema Batch
  {:name s/Str,
   :pareto-front-list [BatchParetoFront]})


(s/defschema ParetoFrontData
  {:single {:saved [NamedParetoFront],
            :latest (s/maybe ParetoFront)}
   :batch [Batch]})


(s/defschema FileData
  {:data {:history [GeneSampleData],
          :position s/Int},
   :pareto-front-data ParetoFrontData,
   :snapshots [Snapshot],
   :plot-style PlotStyle})


(defn validate
  [file-data]
  (s/check FileData file-data))


(defn valid-gene-sample-data?
  [data]
  (s/validate GeneSampleData, data))