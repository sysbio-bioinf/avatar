; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.layout
  (:require [avatar.util :as u]
            [avatar.ui.plot.util :as pu]
            [clojure.string :as str]
            [clj-jfx.core :as jfx])
  (:import (javafx.scene.text Text)))



(defn text-height
  ^double [font, text]
  (-> (doto (Text. (str text))
        (.setFont (jfx/font font)))
    .getBoundsInLocal
    .getHeight))


(defn text-width
  ^double [font, text]
  (-> (doto (Text. text)
        (.setFont (jfx/font font)))
    .getBoundsInLocal
    .getWidth))


(defn gene-group-text-height
  [{:keys [gene-group-font]}, {:keys [group]}]
  (when group
    (text-height gene-group-font, group)))


(defn gene-group-text-width
  [{:keys [gene-group-font]}, {:keys [group]}]
  (when group
    (text-width gene-group-font, group)))


(defn gene-name-text-height
  [{:keys [gene-name-left-font, gene-name-right-font, gene-name-separator]}, {:keys [gene, order-fixed?]}]
  (let [[left-text, _, right-text] (pu/separate-gene-name gene, gene-name-separator)]
    (u/keep-max
      (when order-fixed?
        (text-width (pu/lock-font gene-name-left-font, gene-name-right-font), pu/lock-text))
      (when left-text
        (text-height gene-name-left-font, left-text))
      (when right-text
        (text-height gene-name-right-font, right-text)))))


(defn gene-name-text-width
  [{:keys [gene-name-left-font, gene-name-right-font, gene-name-separator, gene-name-separator-spacing]}, {:keys [gene, order-fixed?]}]
  (let [[left-text, _, right-text] (pu/separate-gene-name gene, gene-name-separator)]
    (->> [(when order-fixed?
            (text-width (pu/lock-font gene-name-left-font, gene-name-right-font), pu/lock-text))
          (when left-text
            (text-width gene-name-left-font, left-text))
          (when right-text (+ (* 2 gene-name-separator-spacing) (text-width (dissoc gene-name-left-font :posture :weight), gene-name-separator)))
          (when right-text
            (text-width gene-name-right-font, right-text))]
      (remove nil?)
      (reduce +))))


(defn alteration-ratio-text-height
  [{:keys [alteration-ratio-font]}]
  (u/reduce-max
    (partial text-height alteration-ratio-font)
    (list* "%" (mapv str (range 10)))))


(defn max-alteration-ratio-text-width
  [{:keys [alteration-ratio-font, alteration-ratio-digits]}]
  (u/keep-max
    (text-width nil, "Mut.")
    (u/reduce-max
      (partial text-width alteration-ratio-font)
      (list*
        (format "100%s%%" (if (pos? alteration-ratio-digits) (apply str "." (repeat alteration-ratio-digits "0")) ""))
        (mapv
          (fn [digit]
            (if (pos? alteration-ratio-digits)
              (format "%d%d.%s%%" digit digit (apply str (repeat alteration-ratio-digits digit)))
              (str digit digit "%")))
          (range 10))))))


(defn max-gene-row-text-height
  [plot-style, gene-list]
  (u/keep-max
    (alteration-ratio-text-height plot-style)
    (u/reduce-max
      (fn [gene]
        (u/keep-max
          (gene-group-text-height plot-style, gene)
          (gene-name-text-height plot-style, gene)))
      gene-list)))

(defn max-gene-group-text-width
  [plot-style, gene-list]
  (u/reduce-max (partial gene-group-text-width plot-style), gene-list))


(defn max-gene-name-text-width
  [plot-style, gene-list]
  (u/reduce-max (partial gene-name-text-width plot-style) gene-list))


(defn max-sample-group-text-height
  [{:keys [sample-group-font] :as plot-style}, sample-group-map]
  (->> sample-group-map
    vals
    (remove str/blank?)
    (u/reduce-max
      (partial text-height sample-group-font))))


(defn plot-text-dimensions
  [{:keys [sample-group-map, gene-list] :as plot-data}, plot-style]
  {:max-sample-group-text-height (max-sample-group-text-height plot-style, sample-group-map),
   :max-gene-name-text-width (max-gene-name-text-width plot-style, gene-list),
   :max-gene-group-text-width (max-gene-group-text-width plot-style, gene-list)
   :max-gene-row-text-height (max-gene-row-text-height plot-style, gene-list)
   :max-alteration-ratio-text-width (max-alteration-ratio-text-width plot-style)})

(def max-text-width 300.0)

(defn calculate-plot-layout
  [{:keys [sample-count, gene-list] :as plot-data},
   {:keys [alteration-cell-width,
           min-row-height,
           min-gene-group-width] :as plot-style},
   show-header?,
   width,
   height]
  (when (and plot-data plot-style)
    (let [{:keys [max-gene-row-text-height,
                  max-sample-group-text-height,
                  max-alteration-ratio-text-width,
                  max-gene-group-text-width,
                  max-gene-name-text-width]} (plot-text-dimensions plot-data, plot-style)
          width (long width),
          height (long height),
          text-margin 3.0,
          dx 10.0,
          dy 10.0,
          scrollbar-width 20.0,
          alteration-row-height (u/keep-max (some-> max-gene-row-text-height (+ dy)) min-row-height),
          row-header-width (if show-header? 60.0 0.0),
          gene-group-width (u/keep-max (some-> max-gene-group-text-width (+ (* 2 text-margin) dx)), min-gene-group-width),
          ; in case there should be no gene name (very unlikely), same treatment as for missing groups
          gene-name-width (-> (u/keep-max (some-> max-gene-name-text-width (+ (* 2 text-margin) dx)), min-gene-group-width)
                            (min max-text-width)),
          max-alteration-ratio-text-width (+ max-alteration-ratio-text-width (* 2 text-margin) dx)
          ; columns
          ; calculate maximal width for alteration cells from available width
          max-alteration-canvas-width (-
                                        width
                                        ; columns: gene group, gene name, alterations, alteration ratio, scrollbar
                                        (cond-> (+
                                                  gene-group-width
                                                  gene-name-width
                                                  max-alteration-ratio-text-width
                                                  scrollbar-width)
                                          show-header?
                                          (+ row-header-width)))
          ; number of visible alteration columns
          alteration-column-count (max 0, (min (long (/ max-alteration-canvas-width alteration-cell-width)), sample-count)),
          ; rows
          sample-group-height (u/keep-max (or (some-> max-sample-group-text-height (+ dy)) 0.0) alteration-row-height)
          gene-count (count gene-list),
          ; calculate maximal height for alteration cells from available height
          column-header-height (if show-header? alteration-row-height 0.0),
          max-alteration-canvas-height (-
                                         height
                                         (cond-> (+
                                                   sample-group-height
                                                   scrollbar-width)
                                           show-header?
                                           (+ column-header-height)))
          alteration-row-count (max 0, (min (long (/ max-alteration-canvas-height alteration-row-height)), gene-count))
          alteration-canvas-width (* alteration-column-count alteration-cell-width)
          alteration-canvas-height (* alteration-row-count alteration-row-height)]
      {:width width,
       :height height
       :image-width (+
                      row-header-width
                      (if max-gene-group-text-width gene-group-width 0.0)
                      (if max-gene-name-text-width gene-name-width 0.0)
                      alteration-canvas-width
                      max-alteration-ratio-text-width)
       :image-height (+
                       column-header-height
                       (if max-sample-group-text-height sample-group-height 0.0)
                       alteration-canvas-height),
       :text-margin text-margin,
       :gene-group-width gene-group-width,
       :gene-name-width gene-name-width,
       :alteration-cell-width alteration-cell-width,
       :alteration-column-count alteration-column-count
       :alteration-canvas-width alteration-canvas-width
       :alteration-ratio-width max-alteration-ratio-text-width,
       :gene-name-x-offset (if max-gene-group-text-width gene-group-width 0.0)
       :alteration-cell-x-offset (cond-> 0.0
                                   max-gene-group-text-width (+ gene-group-width)
                                   max-gene-name-text-width (+ gene-name-width))
       :alteration-cell-y-offset (if max-sample-group-text-height sample-group-height 0.0),
       :scrollbar-width scrollbar-width,
       :sample-group-height sample-group-height,
       :alteration-row-height alteration-row-height,
       :alteration-row-count alteration-row-count,
       :alteration-canvas-height alteration-canvas-height,
       :row-header-width row-header-width,
       :column-header-height column-header-height})))


(defn shorten-text-to-visible-width
  [text, font, width]
  ; if text is not too wide, ...
  (if (<= (text-width font, text) width)
    ; ... then return unmodified
    text
    ; ... else shorten the text.
    (let [n (count text)]
      (loop [i 1 prev ""]
        (if (<= i n)
          (let [shortened (subs text, 0, i)]
            (if (<= (text-width font, shortened) width)
              (recur (inc i), shortened)
              prev))
          text)))))


(defn prepare-gene-texts
  [{:keys [gene-name-left-font, gene-name-right-font, gene-name-separator] :as plot-style},
   gene-name-width,
   order-fixed?,
   gene-name]
  (let [text+font-list (cond->> (mapv
                                  (fn [name-part, font] {:text name-part, :font font})
                                  (pu/separate-gene-name gene-name, gene-name-separator)
                                  [gene-name-left-font, (dissoc gene-name-left-font :posture :weight), gene-name-right-font])
                         order-fixed?
                         (list* {:text pu/lock-text, :font (pu/lock-font gene-name-left-font, gene-name-right-font)})),
        n (count text+font-list)]
    (loop [i 0, current-width 0.0, result-vec (transient [])]
      (if (and (< i n) (< current-width gene-name-width))
        (let [{:keys [text, font] :as text-part} (nth text+font-list i),
              shortened-text (shorten-text-to-visible-width text, font, (- gene-name-width current-width))]
          ; if text has not been shortened, ...
          (if (= text shortened-text)
            ; ... then add text part and continue ...
            (recur
              (unchecked-inc i),
              (+ current-width (text-width font, text)),
              (conj! result-vec text-part))
            ; ... else add shortend text and finish
            (persistent!
              (cond-> result-vec
                ; only add shortened text, if not blank
                (not (str/blank? shortened-text))
                (conj! (assoc text-part :text shortened-text))))))
        (persistent! result-vec)))))