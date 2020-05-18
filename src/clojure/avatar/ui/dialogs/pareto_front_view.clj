; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.pareto-front-view
  (:require [clj-jfx.core :as jfx]
            [avatar.ui.plot :as plot]
            [avatar.data.management :as dm]
            [avatar.data.operations :as ops]
            [avatar.ui.dialogs.gene-sort :as gsd]
            [avatar.ui.tools :as t]
            [avatar.data.gene-data :as gd]
            [clj-jfx.properties :as props]
            [avatar.util :as u]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (javafx.scene.control Label Button ComboBox Accordion)
           (javafx.scene.chart XYChart$Data ScatterChart XYChart$Series)
           (javafx.collections FXCollections)
           (javafx.scene.text Text)
           (javafx.scene.shape Shape)))



(def ^:const point-color "navy")
(def ^:const selected-point-color "orange")


(defn data-point
  ^XYChart$Data [x, y]
  (doto (XYChart$Data. (double x), (double y))
    (.setNode (jfx/circle 5.0, point-color))))


(defn compare-data-point
  [^XYChart$Data point-1, ^XYChart$Data point-2]
  (compare (.getXValue point-1), (.getXValue point-2)))


(defn truncated-x
  ^double [^long factor, ^XYChart$Data point]
  (-> (.getXValue point) (* factor) long double (/ factor)))


(defn chart-data
  [series-name, x-attribute, y-attribute, data-points, x-precision-prop]
  (let [n-1 (dec (count data-points))
        state-atom (atom
                     {:position n-1
                      :max-position n-1
                      :point->solution-map (reduce
                                             (fn [point->solution-map, solution-data]
                                               (let [point (data-point (get solution-data x-attribute), (get solution-data y-attribute))]
                                                 (assoc point->solution-map point solution-data)))
                                             {}
                                             data-points)})
        state-prop (props/data-property state-atom)
        point->solution-map-prop (props/entry-property state-prop, :point->solution-map)

        filtered-points-prop (props/fn-property
                               (fn [x-precision, point->solution-map]
                                 (if x-precision
                                   ; remove duplicates according to truncated coverage
                                   (->> point->solution-map
                                     keys
                                     (group-by (partial truncated-x (long (Math/pow 10.0, x-precision))))
                                     (reduce-kv
                                       (fn [result-set, _, point->solution-list]
                                         (let [min-y-point (apply min-key
                                                             (fn [^XYChart$Data point]
                                                               (.getYValue point))
                                                             point->solution-list)]
                                           (conj! result-set min-y-point)))
                                       (transient #{}))
                                     persistent!)
                                   ; else return unfiltered
                                   (into #{} (keys point->solution-map))))
                               x-precision-prop
                               point->solution-map-prop)
        displayed-points-order-prop (props/fn-property
                                      (fn [filtered-points]
                                        (vec (sort compare-data-point filtered-points)))
                                      filtered-points-prop)
        data-series (doto (XYChart$Series.)
                      (.setName series-name),
                      (.setData (props/observable-array-list (sort compare-data-point (keys (props/get point->solution-map-prop))))))]
    (props/listen-to
      (fn [[old-series-points], [new-series-points]]
        (doseq [^XYChart$Data point (set/difference old-series-points new-series-points)]
          (some-> point ^Shape (.getNode) (.setVisible false)))
        (doseq [^XYChart$Data point (set/difference new-series-points old-series-points)]
          (some-> point ^Shape (.getNode) (.setVisible true))))
      filtered-points-prop)

    (props/bind (props/entry-property state-prop, :filtered-points) filtered-points-prop)
    (props/bind (props/entry-property state-prop, :displayed-points-order) displayed-points-order-prop)
    (props/bind (props/entry-property state-prop, :point->index-map)
      (props/fn-property
        (fn [displayed-points-order]
          (persistent!
            (u/reduce-indexed
              (fn [result-map, index, point]
                (assoc! result-map point index))
              (transient {})
              displayed-points-order)))
        displayed-points-order-prop))

    {:data-series data-series,
     :chart-data-atom state-atom}))


(defn update-selected-solution
  [{:keys [max-position] :as state-map}, new-position]
  (assoc state-map
    :position
    (cond-> new-position max-position (min max-position))))


(defn select-solution
  [selection-state, position]
  (swap! selection-state
    (fn [state-map]
      (update-selected-solution state-map, position))))


(defn prev-solution
  [selection-state]
  (swap! selection-state
    (fn [{:keys [position] :as state-map}]
      (update-selected-solution state-map, (dec position)))))


(defn next-solution
  [selection-state]
  (swap! selection-state
    (fn [{:keys [position] :as state-map}]
      (update-selected-solution state-map, (inc position)))))


(defn setup-scatter-plot
  [^ScatterChart scatter-chart, plot-title, x-label, y-label, {:keys [data-series, chart-data-atom]}]
  (swap! chart-data-atom assoc
    :sort-setup
    {:weighting 0.5,
     :overlap :minimize,
     :by-group? false,
     :sample-group-first? true,
     :show-preview? true,
     :exclude-fixed-rows? true,
     ; ignore fixed genes when sorting samples (since the fixed
     :ignore-fixed-rows-in-column-sort? true})
  (let [chart-data-prop (props/data-property chart-data-atom)
        displayed-points-order-prop (props/entry-property chart-data-prop, :displayed-points-order)
        position-prop (props/entry-property chart-data-prop, :position)
        selected-point-prop (props/entry-property chart-data-prop, :selected-point)
        selected-solution-prop (props/entry-property chart-data-prop, :selected-solution)
        max-position-prop (props/entry-property chart-data-prop, :max-position)
        point->index-map-prop (props/entry-property chart-data-prop, :point->index-map)]
    ; maximal position update
    (props/listen-to
      (fn [_, [displayed-points-order]]
        (let [new-max-position (dec (count displayed-points-order))]
          (swap! chart-data-atom
            (fn [{:keys [position] :as chart-data}]
              (assoc chart-data
                :position (min position new-max-position)
                :max-position new-max-position)))))
      displayed-points-order-prop)
    ; setup highlighting of selected point
    (props/listen-to
      (fn [[^XYChart$Data prev-selected-point], [^XYChart$Data new-selected-point]]
        ; set prev solution objective point to normal point color
        (some-> prev-selected-point .getNode (jfx/shape-fill! point-color))
        ; highlight current solution objective point
        (some-> new-selected-point .getNode (jfx/shape-fill! selected-point-color) jfx/to-front))
      selected-point-prop)
    ; selected point
    (props/bind selected-point-prop
      (props/fn-property
        (fn [displayed-points-order, position, max-position]
          (when (and position max-position)
            (nth displayed-points-order (min position max-position))))
        displayed-points-order-prop
        position-prop
        max-position-prop))
    ; selected solution
    (props/bind selected-solution-prop
      (props/fn-property get
        (props/entry-property chart-data-prop, :point->solution-map)
        selected-point-prop))
    ; gene-sample-data to draw
    (props/bind (props/entry-property chart-data-prop, [:sort-setup, :gene-sample-data])
      (props/fn-property :plot-data, selected-solution-prop))
    ; add series to chart
    (doto scatter-chart
      (.setData (doto (FXCollections/observableArrayList) (.add data-series)))
      (-> .getXAxis (.setLabel x-label))
      (-> .getYAxis (.setLabel y-label))
      (.setLegendVisible false)
      (.setTitle plot-title))
    ; setup data point nodes of the chart
    (doseq [[^XYChart$Data point, solution] (props/get (props/entry-property chart-data-prop, :point->solution-map))]
      (let [{:keys [relative-coverage, relative-overlap]} solution]
        (doto (.getNode point)
          ; display objectives in tooltip
          (jfx/install-tooltip! (format "(%.3f, %.3f)" relative-coverage, relative-overlap))
          ; set on-click handler to select solution
          (.setOnMouseClicked
            (jfx/event-handler
              (fn [_]
                (when-let [position (get (props/get point->index-map-prop) point)]
                  (select-solution chart-data-atom, position))))))))
    chart-data-atom))


; side-effects to save snapshots
(defn save-current-solution-as-snapshot
  [ui, plot]
  (if (deref plot)
    (when-let [snapshot-name (jfx/text-input "Save Snapshot", "Save the current plot", "Enter the snapshot name:")]
      (dm/save-snapshot (:data-management ui), snapshot-name, (plot/data plot), (plot/shown-alteration-type plot)))
    ; error message
    (jfx/show-error "Error", "Save current solution as snapshot", "There is no solution selected!")))


(defn save-pareto-front
  [ui, pareto-front-data]
  (when-let [pareto-front-name (jfx/text-input "Save Pareto Front" "Save the displayed Pareto Front", "Enter the pareto front name:")]
    (dm/save-pareto-front (:data-management ui), pareto-front-name, pareto-front-data)))


(defn kw->str
  [kw]
  (some-> kw name (str/replace #"-" " ")))

(defn setup-parameters-table
  [parameters-table, parameters]
  (let [table-data (atom (->> (dissoc parameters :exclude-fixed-rows?)
                           (mapv (partial zipmap [:parameter-name, :parameter-value]))
                           (sort-by :parameter-name)
                           vec))]
    (jfx/setup-table-view! parameters-table, [:parameter-name, :parameter-value], table-data,
      :sort? true,
      :column-cell-value-fns {:parameter-name kw->str
                              :parameter-value (fn [x]
                                                 (cond-> x
                                                   (keyword? x) kw->str))}))
  )

(defn pareto-front-view
  ([ui, plot-style, pareto-front-data]
   (pareto-front-view ui, plot-style, pareto-front-data, nil))
  ([ui, plot-style, {:keys [pareto-front, hypervolume, parameters] :as pareto-front-data}, pareto-front-name]
   (if (empty? pareto-front)
     (jfx/show-information "Pareto Front View", "The Pareto front is empty."
       "Check whether you chose a too large minimal alteration ratio that removes all genes.")
     (jfx/run-now
       (let [pf-control (jfx/create-control "avatar/ui/ParetoFrontView.fxml"),
             pf-view (jfx/control-node pf-control),
             {:keys [objectives-chart,
                     plot-parent,
                     save-pareto-front-button,
                     save-snapshot-button,
                     ^Button
                     previous-button,
                     ^Button
                     next-button,
                     solutions-label,
                     hypervolume-label,
                     ^Label
                     coverage-label,
                     ^Label
                     overlap-label,
                     ^Label
                     gene-count-label
                     shown-alteration-type-label
                     ^Label
                     solution-index-label
                     ^Text
                     sample-group-text
                     ^Text
                     alteration-type-text
                     settings-parameters-accordion
                     parameters-table
                     ^ComboBox
                     truncation-checkbox
                     precision-slider
                     sort-only-group-combobox]
              :as children} (jfx/control-children pf-control),
             plot (ref nil),
             {:keys [sample-group, second-objective, alteration-type]} parameters,
             x-precision-prop (props/fn-property
                                (fn [truncation?, precision-value]
                                  (when truncation?
                                    (long precision-value)))
                                (props/property truncation-checkbox, :selected)
                                (props/property precision-slider, :value))
             chart-data-atom (setup-scatter-plot objectives-chart, "Gene Selection Results", "coverage",
                               (case second-objective
                                 :overlap "overlap"
                                 :gene-count "selected genes")
                               (chart-data "pareto front", :relative-coverage
                                 (case second-objective
                                   :overlap :relative-overlap
                                   :gene-count :gene-count)
                                 pareto-front
                                 x-precision-prop))
             chart-data-prop (props/data-property chart-data-atom)
             selected-solution-prop (props/entry-property chart-data-prop, [:selected-solution])
             window (doto (jfx/window
                            (cond-> "Pareto Front"
                              pareto-front-name (str (format " \"%s\"" pareto-front-name))),
                            pf-view, 1200, 600)
                      (t/add-avatar-icon))]
         ; truncation checkbox enables corresponding slider
         (props/bind (props/property precision-slider, :disable)
           (props/fn-property not (props/property truncation-checkbox, :selected)))
         ; setup navigation buttons
         (props/bind (props/property previous-button, :disable)
           (props/fn-property
             (fn [position]
               (or (nil? position) (== position 0)))
             (props/entry-property chart-data-prop, :position)))
         (props/bind (props/property next-button, :disable)
           (props/fn-property
             (fn [position, max-position]
               (or (nil? position) (nil? max-position) (== position max-position)))
             (props/entry-property chart-data-prop, :position)
             (props/entry-property chart-data-prop, :max-position)))
         ; solution index
         (props/bind (props/property solution-index-label, :text)
           (props/fn-property
             (fn [position, max-position]
               (if (and position max-position)
                 (format "(Solution %d/%d)" (inc position), (inc max-position))
                 ""))
             (props/entry-property chart-data-prop, :position)
             (props/entry-property chart-data-prop, :max-position)))
         ; set plot-style
         (swap! chart-data-atom update-in [:sort-setup] assoc :plot-style plot-style, :alteration-type alteration-type)
         ; bind sort settings
         (gsd/bind-sort-setting-controls children, chart-data-atom, :sort-setup)
         ; fill parameters table
         (setup-parameters-table parameters-table, parameters)
         ; accordion
         (jfx/change-listener! (props/property settings-parameters-accordion :expanded-pane)
           (fn [_, old-pane, new-pane]
             (when old-pane
               (props/set-property old-pane, :collapsible, true))
             (jfx/run-later
               (when new-pane
                 (props/set-property new-pane, :collapsible, false)))))
         ; set expand first pane
         (when-let [pane (first (.getPanes ^Accordion settings-parameters-accordion,))]
           (props/set-property settings-parameters-accordion, :expanded-pane, pane))
         ; setup plot
         (jfx/handle-event! window, :window-shown,
           (fn [_]
             (gsd/setup-plot (:data-management ui), plot, plot-parent, chart-data-atom, :sort-setup)))
         ; solution coverage
         (props/bind (props/property coverage-label, :text)
           (props/fn-property
             (fn [{:keys [coverage, relative-coverage]}]
               (format "%d (%.3f)" coverage, relative-coverage))
             selected-solution-prop))
         ; solution overlap
         (props/bind (props/property overlap-label, :text)
           (props/fn-property
             (fn [{:keys [overlap, relative-overlap]}]
               (format "%d (%.3f)" overlap, relative-overlap))
             selected-solution-prop))
         ; solution gene count
         (props/bind (props/property gene-count-label, :text)
           (props/fn-property
             (fn [{:keys [plot-data]}]
               (format "%d" (-> plot-data :gene-list count)))
             selected-solution-prop))
         ; shown alteration type
         (props/set-property shown-alteration-type-label :text (gd/alteration-type-str alteration-type))
         ; alteration type
         (props/set-property alteration-type-text, :text, (gd/alteration-type-str alteration-type))
         ; sample group
         (props/set-property sample-group-text, :text, (or sample-group "All Samples"))
         (when sample-group
           (jfx/select-item! sort-only-group-combobox, sample-group))
         ; save pareto front
         (if pareto-front-name
           (jfx/enabled! save-pareto-front-button, false)
           (jfx/handle-event! save-pareto-front-button, :action, (fn [_] (save-pareto-front ui, pareto-front-data))))
         ; save snapshot
         (jfx/handle-event! save-snapshot-button, :action, (fn [_] (save-current-solution-as-snapshot ui, plot)))
         ; set number of solutions
         (jfx/label-text! solutions-label, (format "Solutions: %d" (count pareto-front)))
         (jfx/label-text! hypervolume-label, (format "Hypervolume: %.6f" hypervolume))
         ; previous solution button
         (jfx/handle-event! previous-button
           :action (fn [_] (prev-solution chart-data-atom)))
         ; next solution button
         (jfx/handle-event! next-button
           :action (fn [_] (next-solution chart-data-atom)))
         ; show stage
         (jfx/show window))))))

;(partial update-selected-solution-plot
;plot-style,
;plot-ref,
;(jfx/find-node pf-view, "plot-parent"),
;(jfx/find-node pf-view, "coverage-label")
;(jfx/find-node pf-view, "overlap-label")
;(jfx/find-node pf-view, "genes-label"))
;(partial on-selected-solution-change update-plot-fn)