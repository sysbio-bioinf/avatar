; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.solution-comparison-export
  (:require [avatar.util :as u]
            [clojure.string :as str]
            [avatar.ui.svg :as svg]
            [clj-jfx.colors :as col]
            [avatar.ui.plot.layout :as lo]
            [avatar.data.gene-data :as gd])
  (:import (java.io File)
           (javafx.scene.paint Color)))



(defn calculate-layout
  [{:keys [row-order, column-order, settings] :as table-config}]
  (let [{:keys [solution-column-width
                header-height
                ;gene-font
                selected-snapshot
                objective-rows
                coverage-column-width
                gene-column-width]} settings
        content-row-height 25
        cell-margin 1.0
        text-margin 3.0
        header-text-margin 7.0
        ;gene-column-width (+ (* 2 text-margin)
        ;                    (max
        ;                      (transduce (map (partial lo/text-width gene-font)) max 0.0 row-order)
        ;                      (transduce (map (comp (partial lo/text-width gene-font) gd/objective-text)) max 0.0 objective-rows)))
        width (cond-> (+
                        gene-column-width
                        (* (count column-order) solution-column-width))
                (not= selected-snapshot :coverage-snapshot/none)
                (+ coverage-column-width))
        height (+
                 header-height
                 (* (count row-order) content-row-height)
                 (* (count objective-rows) content-row-height))]
    {:solution-column-width solution-column-width
     :coverage-column-width coverage-column-width
     :content-row-height content-row-height
     :header-height header-height
     :text-margin text-margin
     :header-text-margin header-text-margin
     :gene-column-width gene-column-width
     :cell-margin cell-margin
     :cell-width (- solution-column-width (* 2 cell-margin))
     :cell-height (- content-row-height (* 2 cell-margin))
     :first-gene-row-x 0
     :first-gene-row-y header-height
     :first-cell-x gene-column-width
     :first-cell-y header-height
     :coverage-column-x (+ gene-column-width (* (count column-order) solution-column-width))
     :coverage-column-y header-height
     :first-objective-row-x 0
     :first-objective-row-y (+ header-height (* (count row-order) content-row-height))
     :first-objective-cell-x gene-column-width
     :first-objective-cell-y (+ header-height (* (count row-order) content-row-height))
     :width width
     :height height}))



(defn render-part-of-solution-cell
  [doc
   {:keys [content-row-height
           solution-column-width
           cell-width
           cell-height
           cell-margin
           first-cell-x
           first-cell-y]
    :as layout}
   row-index
   column-index
   {:keys [color] :as solution-settings}]
  (-> (svg/rectangle doc, cell-width, cell-height)
    (svg/position!
      (+ first-cell-x (* column-index solution-column-width) cell-margin)
      (+ first-cell-y (* row-index content-row-height) cell-margin))
    (svg/fill-color! (col/color->str color))))


(defn render-gene-cell
  [doc
   {:keys [first-gene-row-x
           first-gene-row-y
           content-row-height
           gene-column-width
           text-margin]
    :as layout}
   {:keys [gene-font] :as settings}
   row-index,
   gene-name]
  (svg/centered-text-in-rect doc,
    first-gene-row-x
    (+ first-gene-row-y (* row-index content-row-height))
    gene-column-width
    content-row-height
    :start
    gene-font
    text-margin
    (lo/shorten-text-to-visible-width gene-name, gene-font, (- gene-column-width (* 2 text-margin)))))


(defn render-coverage-cell
  [doc
   {:keys [coverage-column-x
           coverage-column-y
           content-row-height
           coverage-column-width
           text-margin]
    :as layout}
   {:keys [coverage-value-font]
    :as settings}
   row-index
   coverage]
  (let [coverage-text (if (or (nil? coverage) (Double/isNaN coverage)) "N/A" (format "%.1f%%" coverage))]
    (svg/centered-text-in-rect doc
      coverage-column-x
      (+ coverage-column-y (* row-index content-row-height))
      coverage-column-width
      content-row-height
      :end
      coverage-value-font,
      text-margin,
      (lo/shorten-text-to-visible-width coverage-text, coverage-value-font, (- coverage-column-width (* 2 text-margin))))))


(defn render-gene-row
  [doc,
   layout,
   {:keys [column-order]
    {solution-settings :solutions
     :as settings} :settings
    :as table-config},
   id->solution,
   gene->coverage,
   row-index,
   gene-name]
  (->> column-order
    (u/reduce-indexed
      (fn [cell-vec, column-index, solution-id]
        (let [{:keys [gene-set]} (get id->solution solution-id)]
          (cond-> cell-vec
            (contains? gene-set gene-name)
            (conj! (render-part-of-solution-cell doc, layout, row-index, column-index, (get solution-settings solution-id))))))
      (transient [(render-gene-cell doc, layout, settings, row-index, gene-name)]))
    persistent!
    (svg/append-elements!
      (-> (svg/group doc)
        (svg/id! (format "%s row" gene-name))
        (cond->
          gene->coverage
          (svg/append! (render-coverage-cell doc, layout, settings, row-index, (get gene->coverage gene-name))))))))


(defn render-all-gene-rows
  [doc, layout, {:keys [row-order, selected-solutions, settings] :as table-config}]
  (let [{:keys [selected-snapshot, selected-alteration-type]} settings
        id->solution (u/key-map :id selected-solutions)
        gene->coverage (when (not= selected-snapshot :coverage-snapshot/none)
                         (gd/gene-coverage-in-snapshot selected-alteration-type, selected-snapshot))]
    (->> row-order
      (u/reduce-indexed
        (fn [row-vec, row-index, gene-name]
          (conj! row-vec (render-gene-row doc, layout, table-config, id->solution, gene->coverage, row-index, gene-name)))
        (transient []))
      persistent!
      (svg/append-elements! (-> (svg/group doc) (svg/id! "gene-rows"))))))


(defn render-objective-text
  [doc
   {:keys [first-objective-row-x
           first-objective-row-y
           content-row-height
           gene-column-width
           text-margin]
    :as layout}
   {:keys [objective-label-font] :as settings}
   row-index
   objective-text]
  (svg/centered-text-in-rect doc
    first-objective-row-x
    (+ first-objective-row-y (* row-index content-row-height))
    gene-column-width
    content-row-height
    :start
    objective-label-font
    text-margin
    (lo/shorten-text-to-visible-width objective-text, objective-label-font, (- gene-column-width (* 2 text-margin)))))


(defn render-objective-value-cell
  [doc
   {:keys [first-objective-cell-x
           first-objective-cell-y
           content-row-height
           solution-column-width
           text-margin]
    :as layout}
   {:keys [objective-value-font] :as settings}
   row-index
   column-index
   objective-value]
  (svg/centered-text-in-rect doc
    (+ first-objective-cell-x (* column-index solution-column-width))
    (+ first-objective-cell-y (* row-index content-row-height))
    solution-column-width
    content-row-height
    :middle
    objective-value-font
    text-margin
    (lo/shorten-text-to-visible-width objective-value, objective-value-font, (- solution-column-width (* 2 text-margin)))))


(defn render-objective-row
  [doc
   layout
   {{:keys [selected-alteration-type, selected-snapshot] :as settings} :settings
    :keys [selected-solutions, column-order]
    :as table-config}
   row-index
   objective-row]
  (let [{:keys [objective-text] :as id->objective}
        (gd/objective-in-snapshot selected-alteration-type, selected-snapshot, selected-solutions, objective-row)]
    (->> column-order
      (u/reduce-indexed
        (fn [cell-vec, column-index, solution-id]
          (let [objective-value (get id->objective solution-id)]
            (conj! cell-vec (render-objective-value-cell doc, layout, settings, row-index, column-index, objective-value))))
        (transient [(render-objective-text doc, layout, settings, row-index, objective-text)]))
      persistent!
      (svg/append-elements!
        (-> (svg/group doc) (svg/id! (format "%s row" objective-text)))))))


(defn render-all-objective-rows
  [doc, layout, table-config, objective-rows]
  (->> objective-rows
    (u/reduce-indexed
      (fn [row-vec, row-index, objective-row]
        (conj! row-vec (render-objective-row doc, layout, table-config, row-index, objective-row)))
      (transient []))
    persistent!
    (svg/append-elements! (-> (svg/group doc) (svg/id! "objective-rows")))))




(defn render-header-cell
  [doc, x, y, width, height, text-margin, rotate?, anchor, label-font, label]
  (if rotate?
    (svg/centered-vertical-text-in-rect doc
      x, y, width, height, anchor, label-font, text-margin,
      (lo/shorten-text-to-visible-width label, label-font, (- height (* 2 text-margin))))
    (svg/centered-text-in-rect doc
      x, y, width, height, anchor, label-font, text-margin,
      (lo/shorten-text-to-visible-width label, label-font, (- width (* 2 text-margin))))))


(defn render-gene-header
  [doc
   {:keys [gene-column-width
           header-height
           header-text-margin]
    :as layout}
   {{:keys [gene-column-label
            gene-column-label-font
            rotate-column-captions?]} :settings
    :as table-config}]
  (render-header-cell doc, 0, 0, gene-column-width, header-height, header-text-margin, rotate-column-captions?,
    (if rotate-column-captions? :start, :middle)
    gene-column-label-font, gene-column-label))


(defn render-solution-header
  [doc
   {:keys [first-cell-x
           solution-column-width
           header-height
           header-text-margin]
    :as layout}
   {{:keys [solution-caption-font
            rotate-column-captions?]} :settings
    :as table-config}
   column-index
   solution-caption]
  (render-header-cell doc
    (+ first-cell-x (* column-index solution-column-width))
    0,
    solution-column-width, header-height, header-text-margin, rotate-column-captions?,
    (if rotate-column-captions? :start, :middle)
    solution-caption-font, solution-caption))


(defn render-coverage-header
  [doc
   {:keys [coverage-column-width
           coverage-column-x
           header-height
           header-text-margin]
    :as layout}
   {{:keys [coverage-label-font
            rotate-column-captions?
            coverage-label]} :settings
    :as table-config}]
  (render-header-cell doc, coverage-column-x, 0, coverage-column-width, header-height, header-text-margin, rotate-column-captions?,
    (if rotate-column-captions? :start, :middle)
    coverage-label-font, coverage-label))


(defn render-header-row
  [doc
   layout
   {:keys [column-order]
    {:keys [selected-snapshot]} :settings
    :as table-config}]
  (let [id->solution-settings (get-in table-config [:settings, :solutions])]
    (cond-> (->> column-order
          (u/reduce-indexed
            (fn [cell-vec, column-index, solution-id]
              (conj! cell-vec
                (render-solution-header doc, layout, table-config, column-index
                  (get-in id->solution-settings [solution-id, :caption]))))
            (transient [(render-gene-header doc, layout, table-config)]))
          persistent!
          (svg/append-elements! (-> (svg/group doc) (svg/id! "header-row"))))
      (not= selected-snapshot :coverage-snapshot/none)
      (svg/append! (render-coverage-header doc, layout, table-config)))))


(defn render-table
  [table-config]
  (let [doc (svg/document)
        {:keys [width, height] :as layout} (calculate-layout table-config)
        {:keys [objective-rows]} (:settings table-config)]
    (-> doc
      (svg/append! (svg/fill-color! (svg/rectangle doc, width, height) (col/color->str Color/WHITE)))
      (svg/append! (render-header-row doc, layout, table-config))
      (svg/append! (render-all-gene-rows doc, layout, table-config))
      (cond->
        (pos? (count objective-rows))
        (svg/append! (render-all-objective-rows doc, layout, table-config, objective-rows)))
      ; size is mandatory
      (svg/document-size! width, height))))


(defn export-table
  [table-config, file]
  (let [doc (render-table table-config)
        ext (u/file-extension file)
        filename (cond-> file (instance? File file) u/file-absolute-path)]
    (case (str/lower-case ext)
      "svg" (svg/save-svg filename, doc),
      "pdf" (svg/save-pdf filename, doc),
      "png" (svg/save-png filename, doc),
      "tiff" (svg/save-tiff filename, doc))))
