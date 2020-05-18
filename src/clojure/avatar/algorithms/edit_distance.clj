; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.algorithms.edit-distance)


(defn double-min
  ^double [^double a, ^double b, ^double c]
  (if (< a b)
    (if (< a c) a c)
    (if (< b c) b c)))


(defn edit-distance
  ^double [^String s1, ^String s2]
  (let [[^String s1, ^String s2] (if (< (.length s1) (.length s2)) [s2 s1] [s1 s2]),
        n1 (.length s1),
        n2 (.length s2),
        prev-dst (make-array Double/TYPE (inc n1)),
        next-dst (make-array Double/TYPE (inc n1))]
    (dotimes [i (inc n1)]
      (aset-double prev-dst i i))
    (loop [j 1, ^doubles prev-dst prev-dst, ^doubles next-dst next-dst]
      (if (> j n2)
        (aget prev-dst n1)
        (do
          (aset-double next-dst 0 j)
          (loop [i 1]
            (let [i-1 (unchecked-dec i)]
              (when (<= i n1)
                (aset-double next-dst i
                  (double-min
                    (+ (aget prev-dst i-1) (let [c1 (.charAt s1 i-1),
                                                 c2 (.charAt s2 (unchecked-dec j))]
                                             (cond
                                               (= c1 c2) 0.0
                                               (= (Character/toUpperCase c1) (Character/toUpperCase c2)) 0.1
                                               :else 1.0)))
                    (+ 1.0 (aget prev-dst i))
                    (+ 1.0 (aget next-dst i-1))))
                (recur (unchecked-inc i)))))
          (recur (unchecked-inc j), next-dst, prev-dst))))))


(defn similarity
  ^double [^String query-str, ^String candidate-str]
  (let [dst (edit-distance query-str, candidate-str)
        n (max (.length query-str) (.length candidate-str))]
    (- 1.0 (/ dst n))))