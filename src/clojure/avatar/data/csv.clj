; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.csv
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [avatar.util :as u])
  (:import
    (clojure.lang IReduceInit)
    (java.io BufferedReader Writer)))


(defn readline
  [^BufferedReader rdr]
  (.readLine rdr))


(defn close
  [^BufferedReader rdr]
  (.close rdr)
  nil)


(defn char-at
  [^String s, ^long i]
  (.charAt s i))


(defn unwrap-quotation
  [quotation, item]
  (let [item (str/trim item),
        n (count item)
        n-1 (dec n)]
    (if (and
          (> n 0)
          (= (char-at item 0) quotation)
          (= (char-at item n-1) quotation))
      (subs item 1, n-1)
      item)))


(defn append-char
  [^StringBuilder sb, c]
  (doto sb
    (.append c)))


(defn split
  [line, separator, quotation]
  (let [line (str/trim line),
        n (count line)]
    (loop [i 0, quoted? false, cell (StringBuilder.) result (transient [])]
      (if (< i n)
        (let [c (char-at line, i)]
          (cond
            (= c quotation)
            (recur (unchecked-inc i), (not quoted?), cell, result)

            (and (= c separator) (not quoted?))
            (recur
              (unchecked-inc i),
              quoted?,
              (StringBuilder.),
              (conj! result (str cell)))

            :else
            (recur
              (unchecked-inc i),
              quoted?,
              (append-char cell, c),
              result)))
        (persistent! (conj! result (str cell)))))))


(defn csv-rows-reducible
  ([readable, separator, quotation, comment]
    (csv-rows-reducible readable, separator, quotation, comment, true))
  ([readable, separator, quotation, comment, close?]
   (let [separator (cond-> separator (string? separator) first)
         quotation (cond-> quotation (string? quotation) first)]
     (reify
       IReduceInit
       (reduce [this, f, start]
         (let [rdr (io/reader readable)]
           (try
             (loop [result start]
               (if-let [line (readline rdr)]
                 ; filter out comments and empty lines
                 (if (or (str/blank? line) (str/starts-with? line comment))
                   (recur result)
                   ; no comment
                   (let [line-vec (split line, separator, quotation)
                         result (f result, line-vec)]
                     (if (reduced? result)
                       ; premature stop, return result
                       (deref result)
                       ; continue
                       (recur result))))
                 ; no lines left, return result
                 result))
             ; always close reader
             (finally
               (when close?
                 (close rdr))))))))))


(defn read-csv
  [readable, & {:keys [separator, quote, comment], :or {separator "," quote "\"", comment "#"}}]
  (persistent!
    (reduce
      conj!
      (transient [])
      (csv-rows-reducible readable, separator, quote, comment))))



(defn writeline
  [^Writer w, line]
  (doto w
    (.write (str line "\n"))))


(defn write-csv
  [writable, rows, & {:keys [separator, quote, comment], :or {separator "," quote "\"", comment "#"}}]
  (let [special-pattern (->> [separator comment]
                          (remove nil?)
                          (str/join)
                          (format "[%s]")
                          (re-pattern))]
    (with-open [w (io/writer writable)]
      (u/for-each!
        (fn [row]
          (let [line (if (string? row)
                       (str comment " " row)
                       (->> row
                         (mapv
                           (fn [cell]
                             (if (and (string? cell) (re-find special-pattern cell))
                               (str quote (str/replace cell, (re-pattern (str quote)) " ") quote)
                               (str/replace (str cell), (re-pattern (str quote)) " "))))
                         (str/join separator)))]
            (writeline w line)))
        rows))))