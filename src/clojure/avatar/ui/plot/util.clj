; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.util
  (:require [clojure.string :as str]))


(defn separate-gene-name
  [^String gene-name, separator]
  (when gene-name
    (let [separator (str separator)]
      (if-let [occurence (str/index-of gene-name, separator)]
        [(.substring gene-name 0, occurence)
         separator
         (.substring gene-name (+ occurence (count separator)))]
        [gene-name]))))


(def lock-text "\uD83D\uDD12 ")

(defn lock-font
  [& fonts]
  (let [size (->> fonts
               (mapv :size)
               (reduce max))]
    {:family "Lucida Sans" :size (+ 2 size)}))


(defn remove-lock-prefix
  [s]
  (if (str/starts-with? s, lock-text)
    (subs s, (count lock-text))
    s))