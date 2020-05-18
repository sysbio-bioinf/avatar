; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.painting
  (:require [avatar.util :as u]
            [clj-jfx.core :as jfx]
            [avatar.ui.plot.structure :as struct]
            [avatar.ui.plot.util :as pu]
            [avatar.ui.plot.layout :as lo]
            [avatar.ui.plot.data :as pdata]
            [clojure.data.int-map :as im]
            [clj-jfx.colors :as col]
            [avatar.algorithms.common :as c]
            [avatar.ui.tools :as t])
  (:import (javafx.scene.layout GridPane)
           (javafx.scene.canvas Canvas GraphicsContext)
           (javafx.scene.control ToggleButton)
           (javafx.scene SnapshotParameters)
           (javafx.scene.paint Color)))





(defn control-visible?
  [^GridPane grid-pane, control]
  (-> grid-pane .getChildren (.contains control)))


(defn show-row-group-cell!
  [{:keys [row-group-grid-pane] :as controls}, control, row-index]
  (when-not (control-visible? row-group-grid-pane, control)
    (jfx/add-to-grid row-group-grid-pane, row-index, 0, control)))


(defn show-column-group-cell!
  [{:keys [column-group-grid-pane] :as controls}, control, column-index]
  (when-not (control-visible? column-group-grid-pane, control)
    (jfx/add-to-grid column-group-grid-pane, 0, column-index, control)))


(defn hide-row-group-cell!
  [{:keys [row-group-grid-pane] :as controls}, control]
  (jfx/remove-children row-group-grid-pane, control))


(defn hide-column-group-cell!
  [{:keys [column-group-grid-pane] :as controls}, control]
  (jfx/remove-children column-group-grid-pane, control))


(defn cell-color
  ([plot-style, group-index]
   (cell-color plot-style, group-index, false, false, false))
  ([{:keys [even-row-color, odd-row-color, alteration-color, overlap-color, missing-value-color, paint-missing-values?] :as plot-style}, group-index, alteration?, already-covered?, missing?]
   (cond
     alteration? (if already-covered? overlap-color alteration-color)
     (and paint-missing-values? missing?) missing-value-color
     (even? group-index) even-row-color
     :else odd-row-color)))


(defn marked-cell-color
  [{:keys [even-row-color, odd-row-color, alteration-color, overlap-color, missing-value-color] :as plot-style}, alteration?, already-covered?, missing?]
  (cond
    alteration? (if already-covered? overlap-color alteration-color)
    missing? missing-value-color
    :else (u/illegal-argument "A marked cell must be either a alteration or a missing value.")))


(defn repaint-gene-group-cell
  [ui-data,
   {:keys [border-color, border-width, gene-group-font] :as plot-style},
   background-color,
   {:keys [group-name-control] :as row},
   row-index,
   group-size,
   in-group-position,
   group]
  (if (#{:first, :single-element} in-group-position)
    (let [group-name-control (doto group-name-control
                               (-> struct/textflow (jfx/textflow-texts! [{:text group, :font gene-group-font}]))
                               (jfx/background-color! background-color)
                               (jfx/region-border! {:color border-color :stroke :solid :widths {:top border-width, :bottom border-width}})
                               (jfx/row-span! group-size))]
      (show-row-group-cell! ui-data, group-name-control, row-index))
    (hide-row-group-cell! ui-data, group-name-control)))


(defn repaint-gene-cell
  [{:keys [border-color, border-width, gene-name-left-font, gene-name-right-font, gene-name-separator, fixed-row-header-color]
    :as plot-style},
   {:keys [gene-name-width] :as plot-layout},
   background-color,
   {:keys [gene-name-control] :as row},
   in-group-position,
   order-fixed?
   gene]
  (let [texts (lo/prepare-gene-texts plot-style, gene-name-width, order-fixed?, gene)]
    (doto gene-name-control
      (-> struct/textflow
        (jfx/textflow-texts! texts))
      (jfx/background-color! background-color)
      (jfx/region-border!
        {:color border-color,
         :stroke :solid
         :widths {:top (when (#{:first, :single-element} in-group-position) border-width),
                  :right border-width
                  :bottom (when (#{:last, :single-element} in-group-position) border-width)}}))))


(defn repaint-alteration-ratio-cell
  [{:keys [border-color, border-width, alteration-ratio-digits, alteration-ratio-font] :as plot-style},
   background-color,
   {:keys [alteration-ratio-control] :as row},
   in-group-position,
   sample-count,
   alterations]
  (let [alteration-count (count alterations)]
    (doto alteration-ratio-control
      (jfx/background-color! background-color)
      (jfx/region-border! {:color border-color,
                           :stroke :solid,
                           :widths {:top (when (#{:first, :single-element} in-group-position) border-width),
                                    :right border-width
                                    :left border-width
                                    :bottom (when (#{:last, :single-element} in-group-position) border-width)}})
      (->
        struct/textflow
        (jfx/textflow-texts!
          [{:text (if (zero? sample-count)
                    "N/A"
                    (format (str "%3." (or alteration-ratio-digits 0) "f%%")
                      (/ (* 100.0 alteration-count) sample-count))),
            :font alteration-ratio-font}])))))


(defn repaint-row
  [controls,
   plot-style,
   plot-layout,
   background-color,
   row,
   row-index,
   group-size,
   in-group-position,
   sample-count,
   {:keys [gene, group, alterations, order-fixed?] :as gene-data}]
  ; group cell
  (repaint-gene-group-cell controls, plot-style, background-color, row, row-index, group-size, in-group-position, group)
  ; gene cell
  (repaint-gene-cell plot-style, plot-layout, background-color, row, in-group-position, order-fixed?, gene)
  ; render alterations per sample
  (repaint-alteration-ratio-cell plot-style, background-color, row, in-group-position, sample-count, alterations)
  nil)


(defn skipped-groups-count
  [view-from-index, data-with-group-coll]
  (->> (subvec data-with-group-coll 0, view-from-index)
    (partition-by :group)
    count))


(defn entities-by-group-in-view
  "For a sorted list of data maps (by index) containing a group return partitions in group
   for the data maps that lie within the view.
    Also return the number of groups that were skipped before the view."
  [view-from-index, view-size, data-with-group-coll]
  (let [view-coll (subvec data-with-group-coll, view-from-index, (+ view-from-index view-size))
        skipped-groups-count (skipped-groups-count view-from-index, data-with-group-coll)
        same-group-at-border? (and
                                (pos? view-from-index)
                                (=
                                  (:group (nth data-with-group-coll (dec view-from-index)))
                                  (:group (nth data-with-group-coll view-from-index))))]
    {:group-offset (cond-> skipped-groups-count same-group-at-border? dec),
     :grouped-view-coll (partition-by :group view-coll)}))


(defn repaint-all-rows
  [{:keys [gene-list, sample-count] :as plot-data},
   plot-style,
   plot-layout,
   {:keys [rows] :as controls},
   {:keys [first-gene-row-index, visible-gene-count] :as view}]
  (let [gene-group-data-coll (mapv
                               (fn [gene-data]
                                 {:group (pdata/gene-group gene-data),
                                  :gene-data gene-data})
                               gene-list)
        {:keys [group-offset, grouped-view-coll]} (entities-by-group-in-view first-gene-row-index, visible-gene-count, gene-group-data-coll),
        visible-index (volatile! 0)]
    (u/for-each-indexed!
      (fn [group-index, group-coll]
        (let [n (count group-coll),
              n-1 (dec n),
              background-color (cell-color plot-style, (+ group-index group-offset))]
          (u/for-each-indexed!
            (fn [element-index, {:keys [gene-data]}]
              (let [position (cond
                               (== n 1) :single-element
                               (zero? element-index) :first
                               (== element-index n-1) :last
                               :else :middle),
                    visible-row-index @visible-index,
                    row (nth rows visible-row-index)]
                (vswap! visible-index inc)
                (repaint-row controls, plot-style, plot-layout, background-color, row, visible-row-index, n, position, sample-count, gene-data)))
            group-coll)))
      grouped-view-coll)))


(defn repaint-column-group-cell
  [controls,
   {:keys [border-color, border-width, sample-group-font, alteration-cell-width] :as plot-style},
   background-color,
   visible-column-index,
   group-size,
   in-group-position,
   group-name,
   group-control]
  (if (#{:first, :single-element} in-group-position)
    (let [available-width (- (* alteration-cell-width group-size) 6.0) ; 2*3.0 spacing
          shortened-group-name (lo/shorten-text-to-visible-width group-name, sample-group-font, available-width)
          group-control (doto group-control
                          (->
                            (jfx/background-color! background-color)
                            (jfx/region-border! {:color border-color :stroke :solid :widths {:right border-width, :left border-width, :bottom border-width}})
                            (jfx/install-tooltip! group-name)
                            (jfx/column-span! group-size))
                          (->
                            struct/textflow
                            (jfx/textflow-texts! [{:text shortened-group-name, :font sample-group-font}])))]
      (show-column-group-cell! controls, group-control, visible-column-index))
    (hide-column-group-cell! controls, group-control)))


(defn repaint-other-columns
  [{:keys [odd-column-group-color, even-column-group-color, border-color, border-width], :as plot-style},
   {:keys [group-name-column-control,
           gene-name-column-control,
           alteration-ratio-column-control] :as columns},
   group-count]
  ; paint cells in upper left corner
  (doto group-name-column-control
    (jfx/background-color! odd-column-group-color)
    (jfx/region-border! {:color border-color :stroke :solid :widths {:bottom border-width}}))
  (doto gene-name-column-control
    (jfx/background-color! odd-column-group-color)
    (jfx/region-border! {:color border-color :stroke :solid :widths {:right border-width, :bottom border-width}}))
  ; paint cell in upper right corner
  (doto alteration-ratio-column-control
    (jfx/background-color! (if (even? group-count) even-column-group-color odd-column-group-color))
    (jfx/region-border! {:color border-color :stroke :solid :widths {:left border-width, :bottom border-width}})))



(defn column-group-color
  [{:keys [odd-column-group-color, even-column-group-color, sample-group->color, sample-groups-colored?], :as plot-style}, group, group-index]
  (if sample-groups-colored?
    ; colored: get color
    (or
      (get sample-group->color group)
      ; fallback to even/odd colors
      (if (even? group-index)
        even-column-group-color
        odd-column-group-color))
    ; color based on group-index
    (if (even? group-index)
      even-column-group-color
      odd-column-group-color)))


(defn repaint-all-columns
  [{:keys [sample-group-map],
    {:keys [column->sample]} :sample-permutation,
    :as plot-data}
   plot-style,
   {:keys [columns] :as controls},
   {:keys [first-sample-column-index, visible-sample-count, sample-count] :as view}]
  (let [{:keys [sample-group-columns]} columns,
        sample-group-data-coll (mapv
                                 (fn [column-index]
                                   (let [sample (get column->sample column-index)]
                                     {:group (get sample-group-map sample)}))
                                 (range sample-count))
        {:keys [grouped-view-coll]} (entities-by-group-in-view first-sample-column-index, visible-sample-count, sample-group-data-coll),
        visible-index (volatile! 0),
        total-group-count (->> sample-group-data-coll (partition-by :group) count)]
    (u/for-each-indexed!
      (fn [group-index, group-coll]
        (let [n (count group-coll),
              n-1 (dec n),
              group-background-color (column-group-color plot-style, (-> group-coll first :group), group-index)]
          (u/for-each-indexed!
            (fn [element-index, {:keys [group]}]
              (let [position (cond
                               (== n 1) :single-element
                               (zero? element-index) :first
                               (== element-index n-1) :last
                               :else :middle),
                    visible-column-index @visible-index
                    group-cell (nth sample-group-columns visible-column-index)]
                (vswap! visible-index inc)
                (repaint-column-group-cell controls, plot-style, group-background-color, visible-column-index, n, position, group, group-cell)))
            group-coll)))
      grouped-view-coll)
    (repaint-other-columns plot-style, columns, total-group-count)))


(defn repaint-alteration-cell
  [^GraphicsContext graphics-context,
   {:keys [border-color, border-width] :as plot-style},
   group-index,
   alteration?,
   already-covered?,
   missing?,
   x,
   y,
   width,
   height]
  (let [fill-color (cell-color plot-style, group-index, alteration?, already-covered?, missing?)
        delta (/ border-width 2.0)]
    (doto graphics-context
      (.setFill (col/to-color fill-color))
      (.fillRect x, y, width, height))
    (when (> border-width 0.0)
      (doto graphics-context
        (.setStroke (col/to-color border-color))
        (.setLineWidth border-width)
        (.strokeRect (+ x delta), (+ y delta), (- width border-width), (- height border-width))))
    graphics-context))


(defn repaint-alterations-row
  [graphics-context,
   {:keys [column->sample] :as sample-permutation},
   plot-style,
   {:keys [first-sample-column-index, visible-sample-count, alteration-cell-height, alteration-cell-width] :as view},
   covered-set,
   group-index,
   row-index,
   {:keys [alterations, missing-values] :as gene-data}]
  (let [y (double (* row-index alteration-cell-height)),
        alteration-cell-width (double alteration-cell-width)
        n (long (+ first-sample-column-index visible-sample-count))]
    (loop [column-index (long first-sample-column-index), x 0.0]
      (when (< column-index n)
        (let [sample-index (nth column->sample column-index),
              alteration? (contains? alterations sample-index),
              already-covered? (contains? covered-set sample-index),
              missing? (contains? missing-values sample-index)]
          (repaint-alteration-cell graphics-context, plot-style, group-index, alteration?, already-covered?, missing?,
            x, y, alteration-cell-width, alteration-cell-height)
          (recur (unchecked-inc column-index), (+ x alteration-cell-width)))))))


(defn repaint-alterations
  [^Canvas canvas,
   {:keys [gene-list, sample-permutation] :as plot-data},
   plot-style,
   {:keys [first-gene-row-index, visible-gene-count] :as view}]
  (let [gene-group-data-coll (mapv
                               (fn [gene-data]
                                 {:group (pdata/gene-group gene-data),
                                  :gene-data gene-data})
                               gene-list),
        ; determine already covered samples
        covered-set (u/reduce-indexed
                      (fn [covered-set, row-index, {:keys [alterations, order-fixed?] :as gene}]
                        (if (< row-index first-gene-row-index)
                          (cond-> covered-set (not order-fixed?) (im/union alterations)),
                          (reduced covered-set)))
                      (im/dense-int-set)
                      gene-list)
        {:keys [group-offset, grouped-view-coll]} (entities-by-group-in-view first-gene-row-index, visible-gene-count, gene-group-data-coll),
        graphics-context (.getGraphicsContext2D canvas),
        row-index-counter (volatile! 0)]
    (u/reduce-indexed
      (fn [covered-set, group-index, group-coll]
        (u/reduce-indexed
          (fn [covered-set, _, {:keys [gene-data]}]
            (let [row-index @row-index-counter]
              (vswap! row-index-counter inc)
              (repaint-alterations-row graphics-context, sample-permutation, plot-style, view, covered-set, (+ group-index group-offset), row-index, gene-data)
              (cond-> covered-set (not (:order-fixed? gene-data)) (im/union (:alterations gene-data)))))
          covered-set
          group-coll))
      covered-set
      grouped-view-coll)))



(defn repaint-headers
  [plot-data, plot-style, view, controls-map, header-type]
  (let [{:keys [gene-list]} plot-data,
        {:keys [fixed-row-header-color]} plot-style,
        headers (get-in controls-map [:headers, header-type, :headers])]
    (u/for-each-indexed!
      (fn [index, ^ToggleButton header]
        (let [data-index (pdata/header-view-index->data-index view, header-type, index)
              header-text (str (inc data-index))]
          (jfx/property-value! header, :text, header-text)
          (cond
            (= header-type :column) (jfx/install-tooltip! header, header-text)
            (= header-type :row) (let [gene (nth gene-list data-index)]
                                   (jfx/property-value! header, :style,
                                     (when (pdata/fixed-gene? gene)
                                       (format "-fx-base: %s;" (-> fixed-row-header-color col/to-color col/color->str))))))))
      headers)))



(defn repaint-alterations-offscreen
  "Paints the alterations offscreen in a fresh canvas each time.
  Avoids slowdown due to canvas history."
  [^Canvas canvas, plot-data, plot-style, view]
  (let [offscreen-canvas (Canvas. (.getWidth canvas), (.getHeight canvas))
        _ (repaint-alterations offscreen-canvas, plot-data, plot-style, view)
        params (doto (SnapshotParameters.)
                 (.setFill Color/TRANSPARENT)),
        image (.snapshot offscreen-canvas, params, nil)]
    (doto (.getGraphicsContext2D canvas)
      (.drawImage image, 0, 0))
    nil))


(defn repaint-plot
  [{:keys [state, controls] :as plot-map}]
  (let [{:keys [plot-data, plot-style, plot-layout, view]} state,
        plot-data (c/select-alteration-data (:shown-alteration-type plot-style), plot-data)
        {:keys [canvas]} controls]
    (when (and plot-data plot-style view controls)
      (repaint-all-rows plot-data, plot-style, plot-layout, controls, view)
      (repaint-headers plot-data, plot-style, view, controls, :row)
      (repaint-all-columns plot-data, plot-style, controls, view)
      (repaint-headers plot-data, plot-style, view, controls, :column)
      (repaint-alterations-offscreen canvas, plot-data, plot-style, view)))
  ; no return value, just side-effects
  nil)
