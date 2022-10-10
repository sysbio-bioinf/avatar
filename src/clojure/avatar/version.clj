; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.version
  (:require
    [clojure.string :as str]))


(def snapshot? false)

(def version [0 4 13])


(defn current-version
  "Returns the current program version as string."
  []
  (cond-> (str/join "." version) snapshot? (str "-SNAPSHOT")))


(defn current-version-data
  "Returns a map containing the version data with the following keys:
  :major, :minor, :patch, :snapshot?"
  []
  (assoc (zipmap [:major, :minor, :patch] version)
    :snapshot? snapshot?))


(defn version-vector
  [version-data]
  (cond
    (map? version-data) (mapv #(get version-data %) [:major, :minor, :patch])
    (vector? version-data) version-data
    ; do not use avatar.util/illegal-argument here since this namespace is loaded in project.clj
    :else (throw (IllegalArgumentException. "A version vector or a map of version data is required!"))))


(defn previous-version?
  "Determine whether the first given version data refers to a previous version compared to the second version data.
  If not specified, the second version data is set to the current version."
  ([version-data]
   (previous-version? version-data, (current-version-data)))
  ([version-1-data, version-2-data]
   (let [result (compare
                  (version-vector version-1-data)
                  (version-vector version-2-data))]
     (neg? result))))


(defn version-str
  "Creates a version string without the snapshot? information.
  Returns version string of current version, when no version data is specified."
  ([]
    (version-str version))
  ([version-data]
   (str/join "." (version-vector version-data))))
