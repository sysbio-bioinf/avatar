; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.manage-clinical-data
  (:require [avatar.ui.dialogs.manage-groups :as mg]
            [avatar.ui.dialogs.group-samples :as gs]
            [avatar.util :as u]
            [frost.quick-freeze :as qf]
            [avatar.data.gene-data :as gd]
            [clojure.data.int-map :as im]
            [clojure.string :as str]
            [avatar.algorithms.common :as co])
  (:import (de.jensd.fx.glyphs.materialdesignicons MaterialDesignIcon)))



(defn collect-original-attributes
  [renaming-map, attribute]
  (persistent!
    (reduce-kv
      (fn [result-vec, original-attribute, mapped-attribute]
        (cond-> result-vec
          (= mapped-attribute attribute)
          (conj! original-attribute)))
      (transient [])
      renaming-map)))


(defn collect-value-frequency-map
  [attributes->values->frequencies, attribute-list]
  (persistent!
    (reduce
      (fn [result-map, attribute]
        (reduce-kv
          (fn [result-map, value, frequency]
            (let [result-freq (get result-map value ::NOT-FOUND)]
              (assoc! result-map
                value (cond-> frequency (not= result-freq ::NOT-FOUND) (+ result-freq)))))
          result-map
          (get attributes->values->frequencies attribute)))
      (transient {})
      attribute-list)))


(defn rename-values
  [value-renaming-map, value-frequency-map]
  (persistent!
    (reduce-kv
      (fn [result-map, dest, src-set]
        ; delete src set
        (cond-> (reduce dissoc! result-map src-set)
          ; if not nil, then add new name with aggregated frequency
          (some? dest)
          (assoc!
            dest
            (reduce
              (fn [sum, src]
                (+ sum (get value-frequency-map src)))
              0
              src-set))))
      (transient value-frequency-map)
      (u/inverse-map value-renaming-map))))


(defn collect-attribute-value-frequency-data
  [attributes->values->frequencies, attribute-renaming-map, value-renaming-map, selected-attribute]
  (->> selected-attribute
    (collect-original-attributes attribute-renaming-map)
    (collect-value-frequency-map attributes->values->frequencies)
    (rename-values (get value-renaming-map selected-attribute))))


(defn perform-renaming
    [renaming-map, entity-merge-fn, entity-map]
    (let [inverse-attribute-renaming-map (-> renaming-map (u/remove-identities) (u/inverse-map))]
      (persistent!
        (reduce-kv
          (fn [result-map, dst-attribute, src-attribute-set]
            (cond-> (reduce dissoc! result-map src-attribute-set)
              dst-attribute
              (assoc!
                dst-attribute
                (transduce
                  (map #(get entity-map %))
                  entity-merge-fn
                  ; if the dst-attribute existed before the renaming, add it to the set for merging
                  (cond-> src-attribute-set
                    (contains? entity-map dst-attribute)
                    (conj dst-attribute))))))
          (transient entity-map)
          inverse-attribute-renaming-map))))


(defn attribute-value-edit-handler
  [value-renaming-data, attribute-dialog-data, [selected-attribute]]
  (let [{:keys [entity-map, renaming-map]} (deref attribute-dialog-data)]
    (swap! value-renaming-data (partial perform-renaming renaming-map, merge))
    (mg/table-edit-dialog
      {:window-title (format "Manage Values of \"%s\"" selected-attribute),
       :entity-map (get entity-map selected-attribute),
       :entity-count-fn identity,
       :entity-merge-fn +,
       :entity-column-name "value",
       :count-column-name "frequency"}
      (fn [value-renaming-map]
        ; update value renaming of selected attribute
        (swap! value-renaming-data update-in [selected-attribute] mg/update-renaming-map value-renaming-map)
        ; update entity-map of attribute dialog
        (swap! attribute-dialog-data update-in [:entity-map, selected-attribute]
          (partial perform-renaming value-renaming-map, +))))))


(defn check-merge
  [attribute-coverage, {:keys [renaming-map]}, selected-groups]
  (let [attribute-coverage (perform-renaming renaming-map, im/union, attribute-coverage),
        selected-groups (vec selected-groups),
        n (count selected-groups),
        overlapping-attributes (for [i (range n),
                                 :let [group-1 (nth selected-groups i),
                                       covered-1 (get attribute-coverage group-1)],
                                 j (range (inc i) n),
                                 :let [group-2 (nth selected-groups j),
                                       covered-2 (get attribute-coverage group-2)],
                                 ; non-empty intersection
                                 :when (pos? (count (im/intersection covered-1 covered-2)))]
                             [group-1 group-2])]
    (when (seq overlapping-attributes)
      {:title "Attribute merge",
       :message "Error: Can not merge attributes.",
       :content (format "The attribute values of the following pairs of attributes overlap:\n%s"
                  (->> overlapping-attributes
                    (map (fn [[group-1, group-2]] (format "%s and %s" group-1, group-2)))
                    (str/join ",\n")))})))


(defn attributes->value-count
  [attributes->values->frequencies]
  (persistent!
    (reduce-kv
      (fn [result-map, attribute, value-data-map]
        (assoc! result-map attribute (count value-data-map)))
      (transient {})
      attributes->values->frequencies)))


(defn value-renaming-without-identities
  [value-renaming-per-attribute]
  (persistent!
    (reduce-kv
      (fn [result-map, attribute, value-renaming]
        (let [value-renaming (u/remove-identities value-renaming)]
          (cond-> result-map
            (seq value-renaming)
            (assoc! attribute value-renaming))))
      (transient {})
      value-renaming-per-attribute)))


(defn show-dialog
  [{:keys [clinical-attributes, clinical-data-map] :as gene-sample-data}, complete-fn]
  (let [attributes->values->frequencies (persistent!
                                          (reduce
                                            (fn [result-map, attribute]
                                              (let [attribute-values-list (gs/collect-attribute-values attribute, clinical-data-map)]
                                                (assoc! result-map attribute
                                                  (-> (zipmap
                                                        (mapv :value attribute-values-list)
                                                        (mapv :frequency attribute-values-list))
                                                    (dissoc co/NOVALUE)))))
                                            (transient {})
                                            clinical-attributes))
        attribute-coverage (gd/clinical-attribute-coverage gene-sample-data),
        value-renaming-data (atom
                              (persistent!
                                (reduce-kv
                                  (fn [result-map, attribute, values->frequencies]
                                    (assoc! result-map attribute
                                      (zipmap (keys values->frequencies) (keys values->frequencies))))
                                  (transient {})
                                  attributes->values->frequencies)))]
    (mg/table-edit-dialog
      {:window-title "Manage Clinical Data",
       :entity-map attributes->values->frequencies,
       :entity-count-fn count,
       :entity-merge-fn (partial merge-with +),
       :entity-column-name "attribute",
       :count-column-name "value count",
       :additional-button {:text "Edit Values",
                           :icon MaterialDesignIcon/SETTINGS,
                           :tooltip "Edit Attribute Values",
                           :disable? (fn [selected-attributes-count] (not= selected-attributes-count 1))
                           :handler (partial attribute-value-edit-handler value-renaming-data)}
       :merge-predicate (partial check-merge attribute-coverage)}
      (fn [attribute-renaming]
        (let [attribute-renaming (u/remove-identities attribute-renaming),
              value-renaming-per-attribute (->> (value-renaming-without-identities (deref value-renaming-data))
                                             (perform-renaming attribute-renaming, merge))]
          (complete-fn attribute-renaming, value-renaming-per-attribute))))))