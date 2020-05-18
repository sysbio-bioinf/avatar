; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.serializers
  (:require
    [frost.serializers :as fs]
    [clojure.data.int-map :as im]
    [avatar.version :as v]
    [frost.quick-freeze :as qf])
  (:import
    (com.esotericsoftware.kryo.io Output Input)))



(defn bitset->bytes
  "Convert bitset to a big integer corresponding to the bit representation of the bitset."
  ^bytes [bs]
  (let [^BigInteger big-int (reduce
                              (fn [^BigInteger big-int, element]
                                (.flipBit big-int element))
                              (BigInteger/valueOf 0)
                              bs)]
    (.toByteArray big-int)))


(defn bytes->bitset
  [^bytes bytes]
  (let [big-int (BigInteger. bytes)
        n (.bitLength big-int)]
    (loop [bit 0, bs (transient (im/dense-int-set))]
      (if (< bit n)
        (recur
          (unchecked-inc bit)
          (cond-> bs (.testBit big-int bit) (conj! bit)))
        (persistent! bs)))))


(defn dense-bitset-serializer
  [meta?]
  (fs/serializer
    (write [kryo, ^Output out, bs]
      (fs/maybe-write-metadata meta? [kryo, out, bs]
        (let [bytes (bitset->bytes bs),
              n (alength bytes)]
          (.writeLong out, n)
          (.writeBytes out, bytes))))
    (read [kryo, ^Input in, clazz]
      (fs/maybe-read-metadata meta? [kryo, in]
        (let [n (.readLong in)
              bytes (.readBytes in, n)]
          (bytes->bitset bytes))))))



(defn save-to-file
  [file, data]
  (qf/quick-file-freeze file, data,
    :additional-serializers [[clojure.data.int_map.PersistentIntSet `dense-bitset-serializer, 200]]
    :file-info {:version (v/current-version-data)}))


(defn resolve-error-handler
  [symbol]
  (when (= symbol 'iggy.data.serializers/dense-bitset-serializer)
    dense-bitset-serializer))


(defn load-from-file
  [file]
  (qf/quick-file-defrost file, :resolve-error-handler resolve-error-handler))


(defn file-info
  [file]
  (qf/quick-file-info file, :resolve-error-handler resolve-error-handler))