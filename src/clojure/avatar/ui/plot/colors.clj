; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.colors
  (:require [avatar.util :as u]
            [clj-jfx.colors :as col]))



(def plot-ui-colors
  #{:missing-value-color
    :even-row-color
    :alteration-color
    :overlap-color
    :selection-color
    :odd-row-color
    :border-color
    :fixed-row-header-color
    :even-column-group-color
    :odd-column-group-color})


(defn plot-style-colors->strs
  [plot-style-map]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k
          (if (= k :sample-group->color)
            (u/update-map-vals v, col/color->str)
            (cond-> v (contains? plot-ui-colors k) col/color->str))))
      (transient {})
      plot-style-map)))


(defn ensure-plot-style-colors
  [plot-style-map]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k
          (if (= k :sample-group->color)
            (u/update-map-vals v, col/to-color)
            (cond-> v (contains? plot-ui-colors k) col/to-color))))
      (transient {})
      plot-style-map)))