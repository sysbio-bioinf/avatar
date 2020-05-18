; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.single-optimization-settings
  (:require [clj-jfx.core :as jfx]
            [avatar.ui.dialogs.gene-sort :as gsd]
            [avatar.util :as u]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clj-jfx.selection :as jfx-select]
            [avatar.ui.tools :as t]
            [avatar.algorithms.common :as c])
  (:import (javafx.scene.control Label TextField Spinner SpinnerValueFactory$IntegerSpinnerValueFactory TextFormatter Button ComboBox SpinnerValueFactory$DoubleSpinnerValueFactory)
           (javafx.geometry Pos)))



(def algorithm-list [{:name "NSGA-II", :id :NSGAII}
                     {:name "Steady-State NSGA-II", :id :SteadyStateNSGAII}
                     {:name "SPEA2", :id :SPEA2}
                     {:name "Global WASF-GA", :id :GWASFGA}])

(defn rounded-long
  [^double n]
  (fn [x]
    (when (number? x)
      (-> (Math/round (/ (double x) n)) (* n) long))))


(defn rounded-double
  [^double n]
  (fn [x]
    (when (number? x)
      (-> (Math/round (/ (double x) n)) (* n)))))


(defn setup-spinner
  [control-map, spinner-kw, dialog-state, property-path, {:keys [min, max-prop, step]}]
  (let [^Spinner spinner (get control-map spinner-kw),
        round (rounded-long step),
        prop (jfx/map-entry-property dialog-state, property-path),
        value-factory (doto (SpinnerValueFactory$IntegerSpinnerValueFactory. min, (jfx/value max-prop), (jfx/value prop), step)
                        (-> .maxProperty (jfx/bind max-prop))),
        editor (.getEditor spinner),
        converter (.getConverter value-factory)]
    ; set value factory
    (.setValueFactory spinner value-factory)
    ; enforce values rounded to step
    (jfx/listen-to
      (fn [value, max]
        (let [rounded (-> (round value) (clojure.core/max min) (clojure.core/min max) int)]
          (when-not (= value rounded)
            (.setValue value-factory rounded)
            (jfx/property-value! editor, :text (.toString converter, rounded)))))
      (jfx/property value-factory, :value)
      max-prop)
    ; sync edit with spinner value (on focus lost) via formatter (cf. https://stackoverflow.com/a/32349847)
    (let [formatter (TextFormatter. converter, (.getValue value-factory))]
      (.setTextFormatter editor formatter)
      (jfx/bind-bidirectional (jfx/property value-factory, :value), (jfx/property formatter, :value)))
    ; bind spinner value to dialog state
    (jfx/bind prop (jfx/property value-factory, :value))
    ; align editor text right
    (jfx/property-value! editor, :alignment, Pos/CENTER_RIGHT)
    spinner))


(defn setup-spinner-double
  [control-map, spinner-kw, dialog-state, property-path, {:keys [min, max-prop, step]}]
  (let [^Spinner spinner (get control-map spinner-kw),
        round (rounded-double step),
        prop (jfx/map-entry-property dialog-state, property-path),
        value-factory (doto (SpinnerValueFactory$DoubleSpinnerValueFactory. min, (jfx/value max-prop), (jfx/value prop), step)
                        (-> .maxProperty (jfx/bind max-prop))),
        editor (.getEditor spinner),
        converter (.getConverter value-factory)]
    ; set value factory
    (.setValueFactory spinner value-factory)
    ; enforce values rounded to step
    (jfx/listen-to
      (fn [value, max]
        (let [rounded (-> (round value) (clojure.core/max min) (clojure.core/min max))]
          (when-not (= value rounded)
            (.setValue value-factory rounded)
            (jfx/property-value! editor, :text (.toString converter, rounded)))))
      (jfx/property value-factory, :value)
      max-prop)
    ; sync edit with spinner value (on focus lost) via formatter (cf. https://stackoverflow.com/a/32349847)
    (let [formatter (TextFormatter. converter, (.getValue value-factory))]
      (.setTextFormatter editor formatter)
      (jfx/bind-bidirectional (jfx/property value-factory, :value), (jfx/property formatter, :value)))
    ; bind spinner value to dialog state
    (jfx/bind prop (jfx/property value-factory, :value))
    ; align editor text right
    (jfx/property-value! editor, :alignment, Pos/CENTER_RIGHT)
    spinner))


(defn setup-simple-tab
  [control-map, dialog-state, gene-count]
  (let [solution-count-prop (jfx/map-entry-property dialog-state, [:simple, :solution-count])]
    ; solution count
    (setup-spinner control-map, :solution-count-spinner, dialog-state, [:simple, :solution-count],
      {:min 10,
       :step 10,
       :max-prop (jfx/object-property 1000)})
    ; selected genes count
    (setup-spinner control-map, :selected-genes-count-spinner, dialog-state, [:simple, :selected-genes-count],
      {:min 1,
       :step 1,
       :max-prop (jfx/object-property gene-count)})
    ; swapped genes count
    (setup-spinner control-map, :swapped-genes-count-spinner, dialog-state, [:simple, :swapped-genes-count],
      {:min 0,
       :step 1,
       :max-prop (jfx/object-property gene-count)})
    ; solution combinations count
    (setup-spinner control-map, :solution-combinations-count-spinner, dialog-state, [:simple, :solution-combinations-count],
      {:min 0,
       :step 1,
       :max-prop (jfx/functional-property
                   (fn [solution-count] (quot solution-count 2))
                   solution-count-prop)})
    ; selection pressure
    (setup-spinner control-map, :selection-pressure-spinner, dialog-state, [:simple, :selection-pressure],
      {:min 2,
       :step 1,
       :max-prop solution-count-prop})
    ; evaluation count
    (setup-spinner control-map, :search-step-count-spinner, dialog-state, [:simple, :search-step-count],
      {:min 10,
       :step 10,
       :max-prop (jfx/object-property 1000000)})))


(defn setup-validated-textfields
  [control-map, dialog-state, validation-state-key & textfield-config-list]
  ; setup validation state
  (swap! dialog-state assoc validation-state-key {:texts {}, :errors {}, :values {}})
  ; configure textfields
  (let [values-prop (jfx/map-entry-property dialog-state, [validation-state-key, :values])]
    (doseq [{:keys [textfield-kw, error-label-kw, property-path, conversion-fn, validator-fn]} textfield-config-list]
      (let [^TextField textfield (get control-map textfield-kw),
            ^Label error-label (get control-map error-label-kw),
            target-value-prop (jfx/map-entry-property dialog-state, property-path),
            initial-value (jfx/value target-value-prop),
            text-prop (jfx/map-entry-property dialog-state, [validation-state-key, :texts, textfield-kw]),
            error-prop (jfx/map-entry-property dialog-state, [validation-state-key, :errors, textfield-kw]),
            value-prop (jfx/map-entry-property dialog-state, [validation-state-key, :values, textfield-kw])]
        ; bind text
        (jfx/bind text-prop, (jfx/property textfield, :text))
        ; bind error
        (jfx/bind (jfx/property error-label, :text), (jfx/functional-property #(some->> % (str "must be ")) error-prop))
        (jfx/bind error-prop,
          (jfx/functional-property validator-fn
            values-prop,
            text-prop))
        ; bind value
        (jfx/bind value-prop
          (jfx/functional-property
            (fn [error, text]
              (when-not error
                (conversion-fn text)))
            error-prop,
            text-prop))
        ; listen to value
        (jfx/listen-to
          (fn [value]
            (when value
              (jfx/value! target-value-prop, value)))
          value-prop)
        ; initial value
        (jfx/property-value! textfield, :text,
          (if (float? initial-value)
            (u/format-double initial-value)
            (str initial-value)))))))


(defn number-validator
  [type, {:keys [is-even?, greater-equal-than, smaller-equal-than]}, values-map, text]
  (assert (#{:integer, :real} type) "type must be either :integer or :real")
  (let [is-even? (and (= type :integer) is-even?),
        integer? (or (not= type :integer) (u/is-long? text)),
        float? (or (not= type :real) (u/is-double? text))
        value (case type
                :integer (when integer? (u/parse-long text))
                :real (when float? (u/parse-double text))),
        even? (or (not is-even?) (and value (even? value))),
        greater-equal-than (if (keyword? greater-equal-than)
                             (get values-map greater-equal-than)
                             greater-equal-than)
        smaller-equal-than (if (keyword? smaller-equal-than)
                             (get values-map smaller-equal-than)
                             smaller-equal-than)
        greater? (or (not greater-equal-than)
                   (and value (>= value greater-equal-than)))
        smaller? (or (not smaller-equal-than)
                   (and value (<= value smaller-equal-than)))]
    (when-not (and integer? float? even? greater? smaller?)
      (str
        (case type
          :integer "an"
          :real "a")
        (when is-even? " even")
        (case type
          :integer " integer"
          :real " real")
        (when (or greater-equal-than smaller-equal-than)
          (str
            ":"
            (when greater-equal-than
              (str " " greater-equal-than " <="))
            " x"
            (when smaller-equal-than
              (str " <= " smaller-equal-than))))))))


(defn setup-combobox
  [^ComboBox combobox, dialog-state, property-path, value->index, index->value-fn, value-list]
  (jfx/property-value! combobox, :items,
    (jfx/observable-array-list value-list))
  (let [prop (jfx/map-entry-property dialog-state, property-path)]
    (jfx/selected-index! combobox, (value->index (jfx/value prop)))
    (jfx/bind prop
      (jfx/functional-property
        index->value-fn
        (jfx/selected-index-property combobox))))
  combobox)


(defn setup-expert-tab
  [control-map, dialog-state]
  (let [{:keys [algorithm-combobox]} control-map]
    ; algorithm choice
    (setup-combobox algorithm-combobox, dialog-state, [:expert, :algorithm],
      ; map from id to index
      (zipmap (mapv :id algorithm-list), (range))
      ; map from index to id
      #(:id (nth algorithm-list %))
      (mapv :name algorithm-list)))

  (setup-validated-textfields control-map, dialog-state, :expert-validation-state,
    ; population size
    {:textfield-kw :population-size-textfield,
     :error-label-kw :population-size-error-label,
     :property-path [:expert, :population-size],
     :conversion-fn u/parse-long,
     :validator-fn (partial number-validator :integer, {:is-even? true, :greater-equal-than 2})},
    ; iteration count
    {:textfield-kw :iteration-count-textfield,
     :error-label-kw :iteration-count-error-label,
     :property-path [:expert, :iteration-count],
     :conversion-fn u/parse-long,
     :validator-fn (partial number-validator :integer, {:greater-equal-than 1})}
    ; gene selection probability
    {:textfield-kw :gene-selection-probability-textfield,
     :error-label-kw :gene-selection-probability-error-label,
     :property-path [:expert, :selection-probability],
     :conversion-fn u/parse-double,
     :validator-fn (partial number-validator :real, {:greater-equal-than 0.0, :smaller-equal-than 1.0})}
    ; mutation probability
    {:textfield-kw :mutation-probability-textfield,
     :error-label-kw :mutation-probability-error-label,
     :property-path [:expert, :mutation-probability],
     :conversion-fn u/parse-double,
     :validator-fn (partial number-validator :real, {:greater-equal-than 0.0, :smaller-equal-than 1.0})}
    ; crossover probability
    {:textfield-kw :crossover-probability-textfield,
     :error-label-kw :crossover-probability-error-label,
     :property-path [:expert, :crossover-probability],
     :conversion-fn u/parse-double,
     :validator-fn (partial number-validator :real, {:greater-equal-than 0.0, :smaller-equal-than 1.0})}
    ; tournament size
    {:textfield-kw :tournament-size-textfield,
     :error-label-kw :tournament-size-error-label,
     :property-path [:expert, :tournament-size],
     :conversion-fn u/parse-long,
     :validator-fn (partial number-validator :integer,
                     {:greater-equal-than 2,
                      :smaller-equal-than :population-size-textfield})}))


(defn setup-algorithm-parameters-control
  [{:keys [parameters-tabpane,
           simple-tab
           expert-tab]
    :as controls},
   initial-state,
   dialog-state]

  ; set initial tab
  (case (:setup-type initial-state)
    :simple (jfx/select-item parameters-tabpane, simple-tab)
    :expert (jfx/select-item parameters-tabpane, expert-tab))

  ; bind :setup-type to selected tab
  (jfx/bind (jfx/map-entry-property dialog-state, [:setup-type])
    (jfx/functional-property
      (fn [selected-item]
        (cond
          (identical? selected-item simple-tab) :simple
          (identical? selected-item expert-tab) :expert))
      (jfx-select/selected-item-property parameters-tabpane)))

  ; simple tab
  (setup-simple-tab controls, dialog-state, (:gene-count initial-state))

  ; expert tab
  (setup-expert-tab controls, dialog-state))


(defn expert-parameters-error?-prop
  [dialog-state]
  (jfx/functional-property
    (fn [setup-type, expert-tab-errors]
      (and
        (= setup-type :expert)
        ; some error detected?
        (->> expert-tab-errors vals (remove nil?) seq boolean)))
    (jfx/map-entry-property dialog-state, [:setup-type])
    (jfx/map-entry-property dialog-state, [:expert-validation-state, :errors])))





(defn initial-parameter-config
  [gene-count]
  {:setup-type :simple,
   :simple {:solution-count 100,
            :selected-genes-count 20,
            :swapped-genes-count 2,
            :solution-combinations-count 5,
            :selection-pressure 10,
            :search-step-count 100},
   :expert {:population-size 100,
            :iteration-count 100,
            :selection-probability 0.1
            :mutation-probability 0.01,
            :crossover-probability 0.02,
            :tournament-size 10,
            :algorithm :NSGAII}})


(defn show-dialog
  [{:keys [gene-list, alteration-type-set] :as gene-sample-data}, prev-optimization-settings, complete-fn]
  (let [gene-count (count gene-list)]
    (if (< gene-count 2)
      (jfx/show-information "Gene Selection Optimization",
        "Too few genes to perform an optimization."
        "At least 2 genes are required.")
      (jfx/run-now
        (let [initial-state (u/deep-merge
                              {:gene-count gene-count,
                               :common {:selected-sample-group "All Samples",
                                        :minimum-alteration-ratio 0.0,
                                        :thread-count (u/cpu-thread-count)}}
                              (initial-parameter-config gene-count)
                              prev-optimization-settings),
              dialog-state (atom initial-state),
              control (jfx/create-control "avatar/ui/SingleOptimizationSettingsDialog.fxml"),
              control-node (jfx/control-node control),
              {:keys [second-objective-combobox,
                      alteration-type-combobox,
                      sample-group-combobox,
                      thread-count-combobox
                      ^Button
                      start-button,
                      cancel-button]
               :as control-map} (jfx/control-children control),
              window (doto (jfx/modal-window "Gene Selection Optimization Setup", control-node)
                       (t/add-avatar-icon)),
              alteration-type-prop (t/setup-alteration-type-combobox alteration-type-combobox, alteration-type-set)
              max-thread-count (u/cpu-thread-count)]

          ; second objective
          (jfx/setup-combobox-first-selected! second-objective-combobox, ["overlap" "gene count"])
          (jfx/bind (jfx/map-entry-property dialog-state, [:common, :second-objective])
            (jfx-select/selected-item-property second-objective-combobox))
          (jfx/select-item! second-objective-combobox, (or (get-in initial-state [:common, :second-objective]) "overlap"))

          ; alteration type
          (jfx/bind (jfx/map-entry-property dialog-state, [:common, :alteration-type]) alteration-type-prop)

          ; sample group selection (sample group is not stored in setup)
          (jfx/setup-combobox-first-selected! sample-group-combobox, (gsd/sample-groups gene-sample-data))
          (jfx/bind (jfx/map-entry-property dialog-state, [:common, :selected-sample-group])
            (jfx-select/selected-item-property sample-group-combobox))

          ; alteration ratio
          (setup-spinner-double control-map, :minimum-alterations-spinner, dialog-state, [:common, :minimum-alteration-ratio],
            {:min 0.0, :max-prop (jfx/object-property 1.0), :step 0.01})

          ; thread count choice
          (setup-combobox thread-count-combobox, dialog-state [:common, :thread-count],
            #(- max-thread-count %),
            #(- max-thread-count %),
            (mapv inc (reverse (range max-thread-count))))

          ; algorithm parameters
          (setup-algorithm-parameters-control control-map, initial-state, dialog-state)

          ; bind start button disable
          (jfx/bind (jfx/property start-button, :disable) (expert-parameters-error?-prop dialog-state))

          (jfx/handle-event! start-button, :action,
            (fn [_]
              (let [{:keys [common] :as setup} (deref dialog-state),
                    {:keys [second-objective,
                            alteration-type,
                            selected-sample-group,
                            minimum-alteration-ratio,
                            thread-count]} common,
                    optimization-settings (merge
                                            {; common
                                             :second-objective (case second-objective
                                                                 "overlap" :overlap
                                                                 "gene count" :gene-count)
                                             :alteration-type alteration-type,
                                             :select-only-for-sample-group (when-not (= selected-sample-group gsd/all-samples)
                                                                             selected-sample-group),
                                             :minimum-alteration-ratio minimum-alteration-ratio,
                                             :thread-count thread-count,
                                             :exclude-fixed-rows? true,
                                             :show-progress? true}
                                            (c/algorithm-parameter-config setup, gene-count))]
                (log/debugf "optimization setup:\n%s" (with-out-str (pp/pprint setup)))
                (complete-fn {:settings optimization-settings,
                              ; do not store sample group
                              :setup (update-in setup [:common] dissoc :selected-sample-group)}))
              (jfx/close window)))

          (jfx/handle-event! cancel-button, :action, (fn [_] (jfx/close window)))
          (jfx/show window))))))