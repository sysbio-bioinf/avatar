; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.z-score)


(defn zscores
  [^doubles values]
  (let [n (alength values)]
    (loop [i 0, sum 0.0, squared-sum 0.0]
      (if (< i n)
        (let [x (aget values i)]
          (recur
            (unchecked-inc i),
            (+ sum x),
            (+ squared-sum (* x x))))
        (let [mean (/ sum n),
              squared-mean (/ squared-sum n)
              stdv (Math/sqrt (- squared-mean (* mean mean)))]
          (loop [j 0, result-values (double-array n)]
            (if (< j n)
              (let [x (aget values j)
                    y (/ (- x mean) stdv)]
                (recur
                  (unchecked-inc j),
                  (doto result-values (aset-double j y))))
              result-values)))))))


(defn values->zscores
  [{:keys [values-per-gene] :as data-map}]
  (assoc data-map
    :values-per-gene
    (mapv #(update-in % [:values] zscores) values-per-gene)))