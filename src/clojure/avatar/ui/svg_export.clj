; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.svg-export
  (:require
    [avatar.ui.svg :as svg]
    [avatar.util :as u]
    [avatar.ui.plot.layout :as lo]
    [clojure.string :as str]
    [avatar.ui.plot.painting :as paint]
    [avatar.ui.plot.util :as pu]
    [clojure.data.int-map :as im]
    [avatar.algorithms.common :as co])
  (:import (java.io File)))




(defn render-left-corner-cell
  [doc,
   {:keys [odd-column-group-color] :as plot-style},
   {:keys [gene-group-width, gene-name-width, sample-group-height, alteration-cell-x-offset] :as plot-layout}]
  (when (or gene-group-width, gene-name-width)
    (-> (svg/rectangle doc, alteration-cell-x-offset, sample-group-height)
      (svg/id! "left-corner-cell")
      (svg/position! 0, 0)
      (svg/fill-color! odd-column-group-color))))


(defn render-right-corner-cell
  [doc,
   {:keys [odd-column-group-color, even-column-group-color] :as plot-style},
   {:keys [alteration-canvas-width, alteration-ratio-width, sample-group-height, alteration-cell-x-offset] :as plot-layout},
   group-count]
  (let [x (+ alteration-cell-x-offset alteration-canvas-width)]
    (-> (svg/rectangle doc, alteration-ratio-width, sample-group-height)
      (svg/id! "right-corner-cell")
      (svg/position! x, 0)
      (svg/fill-color! (if (even? group-count) even-column-group-color odd-column-group-color)))))


(defn render-sample-group-cell
  [doc,
   {:keys [sample-group-font] :as plot-style},
   {:keys [alteration-cell-x-offset, alteration-cell-width, sample-group-height, text-margin] :as plot-layout},
   group-size, group-index, leader-index, group-name]
  (let [x (+ alteration-cell-x-offset (* leader-index alteration-cell-width)),
        y 0.0,
        width (* group-size alteration-cell-width)]
    (-> (svg/group doc)
      (svg/id! (format "sample-group %s" group-name))
      (svg/append! (-> (svg/rectangle doc, width, sample-group-height)
                     (svg/position! x, y)
                     (svg/fill-color!
                       (paint/column-group-color plot-style, group-name, group-index))))

      (cond->
        group-name
        (svg/append!
          (svg/centered-text-in-rect doc, x, y, width, sample-group-height, :middle, sample-group-font, text-margin,
            ; shorten group name to not exceed cell
            (lo/shorten-text-to-visible-width group-name, sample-group-font, (- width (* 2 text-margin)))))))))


(defn sample-group-partitions
  [sample-group-map, {:keys [column->sample] :as sample-permutation}]
  (let [sample-count (count column->sample)]
    (->> (range sample-count)
      (mapv
        (fn [column]
          (let [sample (get column->sample column)]
            (get sample-group-map sample))))
      ; partition cells by group
      (partition-by identity))))


(defn render-sample-group-row
  [doc, plot-style, plot-layout, sample-group-map, sample-permutation]
  (let [left-element (render-left-corner-cell doc, plot-style, plot-layout),
        index (volatile! 0)
        group-partitions (sample-group-partitions sample-group-map, sample-permutation),
        group-count (count group-partitions)
        group-elements (persistent!
                         (u/reduce-indexed
                           (fn [elements, group-index, group]
                             (let [group-size (count group)
                                   leader-index @index]
                               (vswap! index + group-size)
                               (conj! elements
                                 (render-sample-group-cell doc, plot-style, plot-layout, group-size, group-index, leader-index, (first group)))))
                           (transient [])
                           group-partitions))
        right-element (render-right-corner-cell doc, plot-style, plot-layout, group-count)]
    (svg/append-elements! (svg/id! (svg/group doc) "sample-group-row")
      (list* left-element (conj group-elements right-element)))))


(defn render-row-group
  [doc,
   {:keys [gene-group-font] :as plot-style},
   {:keys [image-width, alteration-row-height, alteration-cell-y-offset, gene-group-width, text-margin] :as plot-layout},
   leader-index,
   group-size,
   group-index,
   group-name]
  (let [height (* group-size alteration-row-height),
        x 0,
        y (+ alteration-cell-y-offset (* leader-index alteration-row-height)),
        color (paint/cell-color plot-style, group-index),
        group-element (-> (svg/group doc)
                        (svg/id! (format "gene-group %s" (or group-name group-index))))]
    (-> group-element
      (svg/append! (-> (svg/rectangle doc, image-width, height)
                     (svg/position! x, y)
                     (svg/fill-color! color)))
      (cond->
        group-name
        (svg/append! (svg/centered-text-in-rect doc, x, y, gene-group-width, height, :start, gene-group-font, text-margin, group-name))))))


(defn render-row
  [doc,
   {:keys [gene-name-separator,
           gene-name-separator-spacing,
           gene-name-left-font,
           gene-name-right-font,
           alteration-ratio-digits
           alteration-ratio-font,
           paint-missing-values?] :as plot-style},
   {:keys [gene-group-width,
           alteration-cell-width,
           alteration-row-height,
           alteration-cell-y-offset,
           gene-name-width,
           text-margin,
           image-width,
           alteration-ratio-width,
           alteration-cell-x-offset
           gene-name-x-offset] :as plot-layout},
   {:keys [column->sample], :as sample-permutation},
   covered-set,
   row-index,
   {:keys [gene, order-fixed?, alterations, missing-values] :as gene-data}]
  (let [y (+ alteration-cell-y-offset (* row-index alteration-row-height))
        alteration-ratio-x (- image-width alteration-ratio-width)
        row-element (-> (svg/group doc)
                      (svg/id! (format "gene-row %s" (or gene row-index)))),
        gene-name-text (when gene
                         (svg/centered-text-group-in-rect doc gene-name-x-offset, y, gene-name-width, alteration-row-height,
                           :end, text-margin, gene-name-separator-spacing,
                           ; prepare gene texts: never draw order-fixed? marker since unicode character LOCK
                           ; is not handled well by Apache Batik
                           (lo/prepare-gene-texts plot-style, gene-name-width, false, gene)))
        marked-cells (persistent!
                       (reduce-kv
                         (fn [marked-cells, column-index, sample]
                           (let [alteration? (contains? alterations sample),
                                 already-covered? (contains? covered-set sample),
                                 missing? (contains? missing-values sample)
                                 cell-x (+ alteration-cell-x-offset (* column-index alteration-cell-width))]
                             (cond-> marked-cells
                               (or alteration? (and paint-missing-values? missing?))
                               (conj!
                                 (-> (svg/rectangle doc, alteration-cell-width, alteration-row-height)
                                   (svg/id! (format "gene-%d-sample-%d-covered-%s" row-index, column-index, (if already-covered? "already" "new")))
                                   (svg/position! cell-x, y)
                                   (svg/fill-color! (paint/marked-cell-color plot-style, alteration?, already-covered?, missing?)))))))
                         (transient [])
                         column->sample))
        sample-count (count column->sample),
        alteration-count (count alterations),
        alteration-ratio (/ (* 100.0 alteration-count) sample-count),
        alteration-ratio-text (svg/centered-text-in-rect doc, alteration-ratio-x, y, alteration-ratio-width, alteration-row-height,
                                :end, alteration-ratio-font, text-margin,
                                (if (zero? sample-count)
                                  "N/A"
                                  (format (str "%3." (or alteration-ratio-digits 0) "f%%") alteration-ratio)))]
    (-> row-element
      (cond->
        gene-name-text (svg/append! gene-name-text))
      (svg/append-elements! marked-cells)
      (svg/append! alteration-ratio-text))))


(defn gene-group-partitions
  [gene-list]
  (partition-by (fn [{:keys [group, gene]}] (or group gene)) gene-list))


(defn render-gene-rows
  [doc, plot-style, plot-layout, sample-permutation, gene-list]
  (let [index (volatile! 0)
        covered-set (volatile! (im/dense-int-set))]
    (->> gene-list
      gene-group-partitions
      (u/reduce-indexed
        (fn [group-elements, group-index, group]
          (let [group-size (count group),
                n-1 (dec group-size),
                leader-index @index
                row-group-element (render-row-group doc, plot-style, plot-layout, leader-index, group-size, group-index, (-> group first :group))
                row-elements (persistent!
                               (reduce
                                 (fn [row-elements, gene-data]
                                   (let [row-index @index
                                         row (render-row doc, plot-style, plot-layout, sample-permutation, (deref covered-set), row-index, gene-data)]
                                     (vswap! index inc)
                                     (when-not (:order-fixed? gene-data)
                                       (vswap! covered-set im/union (:alterations gene-data)))
                                     (conj! row-elements row)))
                                 (transient [])
                                 group))]
            (conj! group-elements (svg/append-elements! row-group-element row-elements))))
        (transient []))
      persistent!
      (svg/append-elements! (-> (svg/group doc) (svg/id! "row-groups"))))))


(defn border
  [doc, {:keys [border-color, border-width] :as plot-style}, x1, y1, x2, y2]
  (-> (svg/line doc, x1, y1, x2, y2)
    (svg/stroke! border-color, border-width)))



(defn render-sample-group-borders
  [doc,
   plot-style,
   {:keys [alteration-cell-x-offset,
           alteration-cell-y-offset,
           image-width,
           alteration-cell-width,
           alteration-row-height,
           alteration-canvas-width] :as plot-layout},
   {:keys [sample-group-map, sample-permutation] :as plot-data}]
  (let [group-partitions (sample-group-partitions sample-group-map, sample-permutation),
        index (volatile! 0),
        border-elements (persistent!
                          (reduce
                            (fn [elements, group]
                              (let [group-size (count group),
                                    leader-index @index,
                                    x (+ alteration-cell-x-offset (* leader-index alteration-cell-width))]
                                (vswap! index + group-size)
                                (conj! elements
                                  (border doc, plot-style, x, 0, x, alteration-row-height))))
                            (transient [])
                            group-partitions))
        side-borders [(border doc, plot-style, 0, 0, 0, alteration-row-height)
                      (border doc, plot-style, image-width, 0, image-width, alteration-row-height)]
        top-border (border doc, plot-style, 0, 0, image-width, 0)]
    (-> (svg/group doc)
      (svg/id! "sample-group-borders")
      (svg/append! top-border)
      (svg/append-elements! (conj border-elements
                              ; last group
                              (border doc, plot-style,
                                (+ alteration-cell-x-offset alteration-canvas-width), 0,
                                (+ alteration-cell-x-offset alteration-canvas-width), alteration-row-height)))
      (svg/append-elements! side-borders))))


(defn render-gene-group-borders
  [doc,
   plot-style,
   {:keys [alteration-cell-x-offset,
           alteration-cell-y-offset,
           alteration-row-height,
           image-width,
           image-height,
           alteration-ratio-width] :as plot-layout},
   {:keys [gene-list] :as plot-data}]
  (let [alteration-ratio-x (- image-width alteration-ratio-width)
        index (volatile! 0),
        border-elements (->> gene-list
                          gene-group-partitions
                          (reduce
                            (fn [border-elements, group]
                              (let [group-size (count group),
                                    leader-index @index
                                    y (+ alteration-cell-y-offset (* leader-index alteration-row-height))]
                                (vswap! index + group-size)
                                (-> border-elements
                                  ; group & name border
                                  (cond->
                                    (pos? alteration-cell-x-offset)
                                    (conj! (border doc, plot-style, 0, y, alteration-cell-x-offset, y)))
                                  ; alteration ratio border
                                  (conj! (border doc, plot-style, alteration-ratio-x, y, image-width, y)))))
                            (transient []))
                          persistent!)
        bottom-borders (-> []
                         ; group & name border
                         (cond->
                           (pos? alteration-cell-x-offset)
                           (conj (border doc, plot-style, 0, image-height, alteration-cell-x-offset, image-height)))
                         ; alteration ratio border
                         (conj (border doc, plot-style, alteration-ratio-x, image-height, image-width, image-height)))
        side-borders (-> []
                       ; left
                       (cond->
                         (pos? alteration-cell-x-offset)
                         (conj (border doc, plot-style, 0, alteration-cell-y-offset, 0, image-height)))
                       ;right
                       (conj (border doc, plot-style, image-width, alteration-cell-y-offset, image-width, image-height)))]
    (-> (svg/group doc)
      (svg/id! "gene-group-borders")
      (svg/append-elements! border-elements)
      (svg/append-elements! bottom-borders)
      (svg/append-elements! side-borders))))




(defn render-horizontal-borders
  [doc,
   plot-style,
   {:keys [alteration-cell-x-offset,
           alteration-cell-y-offset,
           alteration-row-count,
           alteration-row-height,
           alteration-canvas-width] :as plot-layout}]
  (let [border-elements (mapv
                          (fn [index]
                            (let [y (+ alteration-cell-y-offset (* index alteration-row-height))]
                              (border doc, plot-style, alteration-cell-x-offset, y, (+ alteration-cell-x-offset alteration-canvas-width), y)))
                          (range (inc alteration-row-count)))]
    (-> (svg/group doc)
      (svg/id! "horizontal-borders")
      (svg/append-elements! border-elements))))


(defn render-vertical-borders
  [doc,
   plot-style
   {:keys [alteration-cell-x-offset,
           alteration-cell-y-offset,
           alteration-column-count,
           alteration-cell-width,
           alteration-canvas-height] :as plot-layout}]
  (let [border-elements (mapv
                          (fn [index]
                            (let [x (+ alteration-cell-x-offset (* index alteration-cell-width))]
                              (border doc, plot-style, x, alteration-cell-y-offset, x, (+ alteration-cell-y-offset alteration-canvas-height))))
                          (range (inc alteration-column-count)))]
    (-> (svg/group doc)
      (svg/id! "vertical-borders")
      (svg/append-elements! border-elements))))


(defn render-borders
  [doc,
   plot-style,
   plot-layout,
   sample-groups?
   plot-data]
  (let [border-group (-> (svg/group doc)
                       (svg/id! "borders"))]
    (-> border-group
      (cond->
        sample-groups?
        (svg/append! (render-sample-group-borders doc, plot-style, plot-layout, plot-data)))
      (svg/append! (render-gene-group-borders doc, plot-style, plot-layout, plot-data))
      (svg/append! (render-horizontal-borders doc, plot-style, plot-layout))
      (svg/append! (render-vertical-borders doc, plot-style, plot-layout)))))


(defn render-plot
  [{:keys [gene-list, sample-permutation, sample-group-map], :as plot-data},
   plot-style]
  (let [sample-groups? (->> sample-group-map vals distinct seq boolean),
        doc (svg/document),
        {:keys [image-width, image-height] :as plot-layout} (lo/calculate-plot-layout plot-data, plot-style, false, Long/MAX_VALUE, Long/MAX_VALUE)
        ; double border width to match visual effect of GUI
        plot-style (update-in plot-style [:border-width] * 2.0)]
    (-> doc
      (svg/append! (svg/rectangle doc, image-width, image-height))
      ; render sample groups if any
      (cond->
        sample-groups?
        (svg/append! (render-sample-group-row doc, plot-style, plot-layout, sample-group-map, sample-permutation)))
      ; render gene rows
      (svg/append! (render-gene-rows doc, plot-style, plot-layout, sample-permutation, gene-list))
      ; render borders
      (svg/append! (render-borders doc, plot-style, plot-layout, sample-groups?, plot-data))
      ; size is mandatory
      (svg/document-size! image-width, image-height))))


(defn export-plot
  [plot-data, plot-style, file]
  (let [plot-data (co/select-alteration-data (:shown-alteration-type plot-style), plot-data)
        doc (render-plot plot-data, plot-style)
        ext (u/file-extension file)
        filename (cond-> file (instance? File file) u/file-absolute-path)]
    (case (str/lower-case ext)
      "svg" (svg/save-svg filename, doc),
      "pdf" (svg/save-pdf filename, doc),
      "png" (svg/save-png filename, doc),
      "tiff" (svg/save-tiff filename, doc))))