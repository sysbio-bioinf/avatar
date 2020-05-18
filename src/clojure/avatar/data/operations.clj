; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.operations
  (:require
    [avatar.data.management :as dm]
    [avatar.util :as u]
    (avatar.algorithms
      [column-sort :as col-sort]
      [mo-select :as mo-select])
    [clojure.string :as str]
    [clojure.data.int-map :as im]
    [avatar.algorithms.common :as co]
    [avatar.algorithms.greedy-setcover-sort :as new-greedy]
    [avatar.algorithms.score :as score]
    [avatar.algorithms.common :as c]
    [avatar.data.gene-data :as gd]
    [avatar.data.import :as imp]
    [clojure.set :as set]))



(defn move-to
  [data-vec, source-index-list, target-index]
  (let [data-vec (u/->vector data-vec),
        n (count data-vec),
        source-index-set (set source-index-list)]
    (loop [i 0, prefix-done? false, prefix (transient []), moving (transient []), postfix (transient [])]
      (if (< i n)
        (let [e (nth data-vec i)]
          (cond
            (contains? source-index-set i)
            (recur (inc i), (if (= i target-index) true prefix-done?), prefix, (conj! moving e), postfix)
            (or (= i target-index) prefix-done?)
            (recur (inc i), true, prefix, moving, (conj! postfix e))
            :else
            ; collect prefix elements
            (recur (inc i), prefix-done?, (conj! prefix e), moving, postfix)))
        (-> prefix (u/into! moving) (u/into! postfix) persistent!)))))


(defn- do-move-genes
  [gene-sample-data, source-index-list, target-index]
  (let [gene-list (:gene-list gene-sample-data),
        all-fixed? (every? #(:order-fixed? (nth gene-list %)) source-index-list),
        ; moving the selected genes above a fixed gene is not allowed
        target-index (if all-fixed?
                       target-index
                       ; count fixed genes, assumes that fixed genes must be at the top of the list
                       (let [fixed-gene-count (reduce
                                                (fn [n, {:keys [order-fixed?]}]
                                                  (if order-fixed?
                                                    (inc n)
                                                    (reduced n)))
                                                0
                                                gene-list)]
                         (max fixed-gene-count target-index))),
        new-gene-list (move-to gene-list, source-index-list, target-index)]
    (assoc gene-sample-data :gene-list new-gene-list)))


(defn move-genes
  [data-mgmt, source-index-list, target-index]
  (dm/modify-current-data data-mgmt do-move-genes source-index-list, target-index))


(defn- do-move-samples
  [gene-sample-data, source-index-list, target-index]
  (let [{{:keys [column->sample, sample->column]} :sample-permutation} gene-sample-data,
        new-column->sample (move-to column->sample, source-index-list, target-index)]
    (-> gene-sample-data
      (assoc-in [:sample-permutation, :column->sample] new-column->sample)
      (assoc-in [:sample-permutation, :sample->column] (u/inverse-permutation new-column->sample)))))


(defn move-samples
  [data-mgmt, source-index-list, target-index]
  (dm/modify-current-data data-mgmt do-move-samples source-index-list, target-index))


(defn greedy-setcover-sorting
  [{:keys [gene-list, sample-group-map] :as gene-sample-data},
   {:keys [weighting, by-group?, sample-group-first?, overlap, exclude-fixed-rows?, ignore-fixed-rows-in-column-sort?,
           only-sample-group, alteration-type] :as setup-data}]
  (let [id->gene-map (zipmap (mapv :gene-id gene-list) gene-list),
        input-gene-list (co/maybe-remove-sample-alterations-of-other-groups only-sample-group, sample-group-map,
                          (:gene-list (c/select-alteration-data alteration-type, gene-sample-data))),
        new-gene-order (u/timing "deterministic greedy gene sorting"
                         (new-greedy/greedy-sort
                           {:weighting weighting,
                            :overlap-objective overlap,
                            :group? by-group?,
                            :preselected-candidates
                            ; seed with genes whose order has been fixed (except when they shall be excluded)
                            (when-not exclude-fixed-rows?
                              (filterv :order-fixed? input-gene-list))}
                           ; remove fixed genes, if they shall be excluded
                           (cond-> input-gene-list
                             exclude-fixed-rows?
                             (->> (remove :order-fixed?) vec))))
        new-gene-list (cond-> (mapv (fn [{:keys [gene-id]}] (get id->gene-map gene-id)) new-gene-order)
                        exclude-fixed-rows?
                        (->> (concat (filter :order-fixed? gene-list)) vec))
        new-gene-sample-data (assoc gene-sample-data :gene-list new-gene-list)]
    (u/timing "lexicographic sample sort"
      (col-sort/lexicographic-sort sample-group-first?, ignore-fixed-rows-in-column-sort?, alteration-type, new-gene-sample-data))))


(defn setcover-based-sorting
  [data-mgmt, setup-data]
  (dm/modify-current-data data-mgmt, greedy-setcover-sorting, setup-data))







(defn sort-rows-and-columns
  [data-mgmt, {:keys [type] :as setup-data}]
  (setcover-based-sorting data-mgmt, setup-data))


(defn sort-columns
  [data-mgmt, sample-group-first?, alteration-type]
  (dm/modify-current-data data-mgmt
    (fn [gene-sample-data]
      (u/timing "lex. column sort"
        (col-sort/lexicographic-sort sample-group-first?, false, alteration-type, gene-sample-data)))))


(defn find-gene-group-start
  ^long [gene-list, given-group, ^long current-index]
  (loop [index current-index]
    (if (<= 0 index)
      (let [{:keys [group]} (nth gene-list index)]
        (if (= group given-group)
          ; same group, continue
          (recur (unchecked-dec index))
          ; different groups, previous index is the last belonging to the group
          (unchecked-inc index)))
      0)))


(defn find-gene-group-end
  ^long [gene-list, given-group, ^long current-index]
  (let [n (count gene-list)]
    (loop [index current-index]
      (if (< index n)
        (let [{:keys [group]} (nth gene-list index)]
          (if (= group given-group)
            ; same group, continue
            (recur (unchecked-inc index))
            ; different groups, previous index is the last belonging to the group
            (unchecked-dec index)))
        (unchecked-dec n)))))


(defn do-rename-gene-group-block
  [gene-sample-data, row-index, group-name]
  (let [group-name (when-not (str/blank? group-name) group-name)
        gene-list (:gene-list gene-sample-data),
        old-group-name (:group (nth gene-list row-index))]
    (assoc gene-sample-data
      :gene-list
      (if old-group-name
        ; had a group name before, rename the group of all surrounding group members
        (let [start-index (find-gene-group-start gene-list, old-group-name, row-index),
              end-index (find-gene-group-end gene-list, old-group-name, row-index),
              gene-list (reduce
                          (fn [gene-list, index]
                            (assoc-in gene-list [index, :group] group-name))
                          gene-list
                          (range start-index (unchecked-inc end-index)))]
          gene-list)
        ; no group name before, just rename the group of the selected gene
        (assoc-in gene-list [row-index, :group] group-name)))))


(defn rename-gene-group-block
  [data-mgmt, row-index, group-name]
  (dm/modify-current-data data-mgmt, do-rename-gene-group-block, row-index, group-name))


(defn rename-gene
  [data-mgmt, row-index, gene-name]
  (dm/modify-current-data data-mgmt
    (fn [gene-sample-data]
      (assoc-in gene-sample-data [:gene-list, row-index, :gene] gene-name))))


(defn find-sample-group-start
  ^long [sample-group-map, column->sample, given-group, ^long current-column-index]
  (loop [column-index current-column-index]
    (if (<= 0 column-index)
      (let [group (get sample-group-map (column->sample column-index))]
        (if (= group given-group)
          (recur (unchecked-dec column-index))
          (unchecked-inc column-index)))
      0)))


(defn find-sample-group-end
  ^long [sample-group-map, column->sample, given-group, ^long current-column-index]
  (let [n (count column->sample)]
    (loop [column-index current-column-index]
      (if (< column-index n)
        (let [group (get sample-group-map (column->sample column-index))]
          (if (= group given-group)
            (recur (unchecked-inc column-index))
            (unchecked-dec column-index)))
        (unchecked-dec n)))))


(defn do-rename-sample-group-block
  [gene-sample-data, column-index, group-name]
  (let [group-name (when-not (str/blank? group-name) group-name)]
    (update-in gene-sample-data [:sample-group-map]
      (fn [sample-group-map]
        (let [n (:sample-count gene-sample-data),
              {:keys [column->sample]} (:sample-permutation gene-sample-data),
              old-group-name (get sample-group-map (column->sample column-index)),
              start-index (find-sample-group-start sample-group-map, column->sample, old-group-name column-index),
              end-index (find-sample-group-end sample-group-map, column->sample, old-group-name column-index)]
          ; rename the complete block with the same group name (or the block with no group name)
          (reduce
            (fn [sample-group-map, index]
              (assoc sample-group-map (column->sample index) group-name))
            sample-group-map
            (range start-index (unchecked-inc end-index))))))))


(defn rename-sample-group-block
  [data-mgmt, column-index, group-name]
  (dm/modify-current-data data-mgmt, do-rename-sample-group-block, column-index, group-name))


(defn do-change-groups
  [gene-sample-data, selected-rows, group-name]
  (let [group-name (when-not (str/blank? group-name) group-name)]
    (update-in gene-sample-data [:gene-list]
      #(reduce
         (fn [gene-list, row-index]
           (update-in gene-list [row-index] assoc :group group-name))
         %
         selected-rows))))


(defn change-gene-groups
  [data-mgmt, selected-rows, group-name]
  (dm/modify-current-data data-mgmt, do-change-groups, selected-rows, group-name))


(defn change-sample-groups
  [data-mgmt, selected-indices, group-name]
  (let [group-name (when-not (str/blank? group-name) group-name)]
    (dm/modify-current-data data-mgmt
      (fn [{{:keys [column->sample]} :sample-permutation, :as gene-sample-data}]
        (update-in gene-sample-data [:sample-group-map]
          (fn [sample-group-map]
            (reduce
              #(assoc %1 (nth column->sample %2) group-name)
              sample-group-map
              selected-indices)))))))



(defn do-delete-selected-genes
  [gene-sample-data, selected-genes-indices]
  (update-in gene-sample-data [:gene-list] #(u/delete selected-genes-indices, %)))


(defn delete-selected-genes
  [data-mgmt, selected-genes-indices]
  (dm/modify-current-data data-mgmt, do-delete-selected-genes, selected-genes-indices))


(defn do-filter-selected-genes
  [gene-sample-data, selected-genes-indices]
  (update-in gene-sample-data [:gene-list] #(u/filter-by-index selected-genes-indices, %)))


(defn filter-selected-genes
  [data-mgmt, selected-genes-indices]
  (dm/modify-current-data data-mgmt, do-filter-selected-genes, selected-genes-indices))


(defn delete-samples-by-selected-columns
  [data-mgmt, selected-columns]
  (dm/modify-current-data data-mgmt,
    (fn [{:keys [sample-permutation] :as gene-sample-data}]
      (let [{:keys [column->sample]} sample-permutation]
        (gd/delete-samples gene-sample-data, (mapv column->sample selected-columns))))))


(defn fix-gene-order
  [gene-sample-data, selected-rows]
  (let [selected-rows (u/->set selected-rows)]
    (update-in gene-sample-data [:gene-list]
      (fn [gene-list]
        (let [n (count gene-list)]
          (loop [i 0, already-fixed (transient []), just-fixed (transient []), others (transient [])]
            (if (< i n)
              (let [gene (nth gene-list i)]
                (cond
                  (:order-fixed? gene)
                  (recur
                    (inc i),
                    (conj! already-fixed gene),
                    just-fixed,
                    others)
                  (contains? selected-rows i)
                  (recur
                    (inc i),
                    already-fixed,
                    (conj! just-fixed (assoc gene :order-fixed? true)),
                    others)
                  :else
                  (recur
                    (inc i),
                    already-fixed,
                    just-fixed,
                    (conj! others gene))))
              (-> already-fixed (u/into! just-fixed) (u/into! others) persistent!))))))))


(defn unfix-gene-order
  [gene-sample-data, selected-rows]
  (let [selected-rows (u/->set selected-rows)]
    (update-in gene-sample-data [:gene-list]
      (fn [gene-list]
        (let [n (count gene-list)]
          (loop [i 0, still-fixed (transient []), just-unfixed (transient []), others (transient [])]
            (if (< i n)
              (let [gene (nth gene-list i)]
                (cond
                  (not (:order-fixed? gene))
                  (recur
                    (inc i),
                    still-fixed,
                    just-unfixed,
                    (conj! others gene))
                  (contains? selected-rows i)
                  (recur
                    (inc i),
                    still-fixed,
                    (conj! just-unfixed (assoc gene :order-fixed? false)),
                    others)
                  :else
                  (recur
                    (inc i),
                    (conj! still-fixed gene),
                    just-unfixed,
                    others)))
              (-> still-fixed (u/into! just-unfixed) (u/into! others) persistent!))))))))


(defn modify-fixed-order-of-genes
  [data-mgmt, selected-rows, fix?]
  (dm/modify-current-data data-mgmt, (if fix? fix-gene-order unfix-gene-order), selected-rows))


(defn move-genes-to-front
  [gene-sample-data, selected-rows]
  (let [selected-rows (u/->set selected-rows)]
    (update-in gene-sample-data [:gene-list]
      (fn [gene-list]
        (let [n (count gene-list)]
          (loop [i 0, to-move (transient []), others (transient [])]
            (if (< i n)
              (let [gene (nth gene-list i)]
                (if (contains? selected-rows i)
                  (recur (unchecked-inc i), (conj! to-move gene), others)
                  (recur (unchecked-inc i), to-move, (conj! others gene))))
              (persistent! (u/into! to-move others)))))))))


(defn move-to-front
  [data-mgmt, header-type, selection]
  (case header-type
    :row (move-genes data-mgmt, selection, 0)
    :column (move-samples data-mgmt, selection, 0))
  data-mgmt)


(defn delete-genes-without-alterations
  [data-mgmt, alteration-types]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (do-delete-selected-genes gene-sample-data,
        (gd/find-genes-without-alterations alteration-types, gene-sample-data)))))


(defn delete-genes
  [data-mgmt, minimal-alterations, maximal-alterations, alteration-types]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (do-delete-selected-genes gene-sample-data,
        (gd/find-genes-outside-alteration-count-range alteration-types, minimal-alterations, maximal-alterations, gene-sample-data)))))


(defn group->samples-map
  [sample-group-map]
  (persistent!
    (reduce-kv
      (fn [result-map, sample, group]
        (u/update-in! result-map [group] (fnil conj (im/dense-int-set)) sample))
      (transient {})
      sample-group-map)))


(defn delete-samples-without-alterations
  [data-mgmt, alteration-types]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (gd/delete-samples gene-sample-data,
        (gd/find-samples-without-alterations alteration-types, gene-sample-data)))))


(defn delete-samples-without-clinical-data
  [data-mgmt]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (gd/delete-samples gene-sample-data,
        (gd/find-samples-without-clinical-data gene-sample-data)))))


(defn assign-sample-group
  [gene-sample-data, sample-group]
  (if (str/blank? sample-group)
    gene-sample-data
    (let [{:keys [sample-count]} gene-sample-data]
      (assoc gene-sample-data :sample-group-map (zipmap (range sample-count) (repeat sample-group))))))



(defn merge-sample-data-maps
  "Merge the two given sample data maps.
  The ids of samples and columns of the additional samples are shifted by the number of current samples."
  [sample-data-attribute,
   {current-sample-count :sample-count
    :as current-gene-sample-data},
   additional-gene-sample-data]
  (reduce-kv
    (fn [sample-data-map, old-sample-id, sample-data]
      ; store data under new sample id
      (assoc sample-data-map (+ old-sample-id current-sample-count) sample-data))
    (get current-gene-sample-data sample-data-attribute)
    (get additional-gene-sample-data sample-data-attribute)))


(defn merge-sample-permutations
  "Merge the two given sample permutations."
  [current-sample-permutation, additional-sample-permutation]
  (let [current-sample->column (:sample->column current-sample-permutation)
        current-sample-count (count current-sample->column)
        new-sample->column (into current-sample->column
                             (mapv
                               (fn [prev-column]
                                 ; additional samples get columns after the current samples
                                 (+ current-sample-count prev-column))
                               (:sample->column additional-sample-permutation)))]
    {:sample->column new-sample->column
     :column->sample (u/inverse-permutation new-sample->column)}))


(defn reassign-gene-ids
  [gene-list]
  (mapv
    (fn [id, gene]
      (assoc gene :gene-id id))
    (range)
    gene-list))


(defn merge-alterations-add-samples
  [current-sample-count, additional-sample-count, current-alteration-map, additional-alteration-map]
  (let [current-alterations (:alterations current-alteration-map),
        additional-alterations (some->> (:alterations additional-alteration-map)
                                 (reduce
                                   (fn [alteration-set, sample]
                                     (conj alteration-set (+ sample current-sample-count)))
                                   (im/dense-int-set))),
        current-missing-values (:missing-values current-alteration-map),
        additional-missing-values (some->> (:missing-values additional-alteration-map)
                                    (reduce
                                      (fn [s, sample]
                                        (conj s (+ sample current-sample-count)))
                                      (im/dense-int-set)))
        total-sample-count (+ current-sample-count additional-sample-count)]
    {:alterations (if current-alteration-map
                    (into current-alterations additional-alterations)
                    (or additional-alterations (im/dense-int-set))),
     :missing-values (if current-alteration-map
                       (if additional-alteration-map
                         (into current-missing-values additional-missing-values)
                         (into current-missing-values (range current-sample-count total-sample-count)))
                       (if additional-alteration-map
                         (into (im/dense-int-set (range current-sample-count)) additional-missing-values)
                         ; all values missing
                         (im/dense-int-set (range total-sample-count))))}))

(defn merge-gene-data-add-samples
  [merged-alteration-types, current-sample-count, additional-sample-count, gene-name, current-gene-data, additional-gene-data]
  {:gene gene-name,
   :group (or (:group current-gene-data) (:group additional-gene-data)),
   :order-fixed? (boolean (:order-fixed? current-gene-data))
   :alteration-data (reduce
                      (fn [result-map, alteration-type]
                        (assoc result-map
                          alteration-type
                          (merge-alterations-add-samples
                            current-sample-count,
                            additional-sample-count,
                            (get-in current-gene-data [:alteration-data, alteration-type]),
                            (get-in additional-gene-data [:alteration-data, alteration-type]))))
                      {}
                      merged-alteration-types)})


(defn merge-data-add-samples
  "Inserts the new gene-sample-data into the current data.
  Additional genes may be provided by the new data."
  [current-gene-sample-data, additional-gene-sample-data]
  (let [{current-gene-list :gene-list,
         current-sample-ids :sample-id-vec,
         current-sample-count :sample-count} current-gene-sample-data,
        {additional-gene-list :gene-list,
         additional-sample-ids :sample-id-vec,
         additional-sample-count :sample-count} additional-gene-sample-data,
        merged-sample-count (+ current-sample-count additional-sample-count)
        merged-alteration-types (into (:alteration-type-set current-gene-sample-data) (:alteration-type-set additional-gene-sample-data)),
        clinical-data-map (merge-sample-data-maps :clinical-data-map, current-gene-sample-data, additional-gene-sample-data)]
    {:sample-count merged-sample-count
     :sample-permutation (merge-sample-permutations (:sample-permutation current-gene-sample-data), (:sample-permutation additional-gene-sample-data))
     :sample-group-map (merge-sample-data-maps :sample-group-map, current-gene-sample-data, additional-gene-sample-data),
     :sample-id-vec (into current-sample-ids additional-sample-ids),
     :clinical-attributes (imp/clinical-attributes clinical-data-map),
     :clinical-data-map clinical-data-map
     :alteration-type-set merged-alteration-types,
     :gene-list (let [current-gene-map (zipmap (mapv :gene current-gene-list) current-gene-list),
                      additional-gene-map (zipmap (mapv :gene additional-gene-list) additional-gene-list),
                      all-genes (->> (concat current-gene-list additional-gene-list)
                                  (mapv :gene)
                                  distinct
                                  vec)]
                  (reassign-gene-ids
                    (mapv
                      (fn [gene-name]
                        (let [current-gene-data (get current-gene-map gene-name),
                              additional-gene-data (get additional-gene-map gene-name)]
                          (merge-gene-data-add-samples merged-alteration-types, current-sample-count, additional-sample-count, gene-name, current-gene-data, additional-gene-data)))
                      all-genes)))}))


(defn insert-gene-sample-data-unconstrained
  ([data-mgmt, additional-gene-sample-data]
   (dm/modify-current-data data-mgmt, merge-data-add-samples, additional-gene-sample-data))
  ([data-mgmt, additional-gene-sample-data, sample-group]
   (insert-gene-sample-data-unconstrained data-mgmt,
     (assign-sample-group additional-gene-sample-data, sample-group))))


(defn sample-id-merge
  [current-sample-ids, additional-sample-ids]
  (let [current-sample-id-map (zipmap current-sample-ids (range))
        current-sample-count (count current-sample-ids)
        n (count additional-sample-ids)]
    (loop [index 0, new-index current-sample-count, duplicate-set (transient #{}), index-mapping (transient {})]
      (if (< index n)
        (let [sample-id (nth additional-sample-ids index)]
          (let [old-index (get current-sample-id-map sample-id)]
            (recur
              (unchecked-inc index)
              ; increment new-index if a new sample has been encountered
              (cond-> new-index (not old-index) unchecked-inc)
              ; old-index != nil => duplicate
              (cond-> duplicate-set old-index (conj! index))
              ; store old-index for duplicate and new index for new samples
              (assoc! index-mapping index (or old-index new-index)))))
        {:duplicates (persistent! duplicate-set)
         :index-mapping (persistent! index-mapping)
         ; distinct sample ids
         :sample-ids (into [] (comp cat (distinct)) [current-sample-ids, additional-sample-ids])}))))


(defn merge-sample-permutations-without-duplicates
  "Merge the two given sample permutations."
  [current-sample-permutation, additional-sample-permutation, duplicates, index-mapping]
  (let [new-column->sample (into (:column->sample current-sample-permutation)
                             (comp
                               (remove duplicates)
                               (map index-mapping))
                             (:column->sample additional-sample-permutation))]
    {:column->sample new-column->sample
     :sample->column (u/inverse-permutation new-column->sample)}))


(defn combine-data
  [old-data, new-data, append-only?]
  (if (and (map? old-data) (map? new-data))
    (if append-only?
      ; do not replace existing keys in old-data
      (u/deep-merge-append-only old-data, new-data)
      ; do not delete values of existing keys in old-data
      (u/deep-merge-non-delete old-data, new-data))
    (if append-only?
      ; use old-data if non-nil
      (if (nil? old-data) new-data old-data)
      ; overwrite old-data if new-data is non-nil
      (if (nil? new-data) old-data new-data))))


(defn merge-sample-data-maps-without-duplicates
  "Merge the two given sample data maps.
  The ids of samples and columns of the additional samples are mapped as specified."
  [sample-data-attribute,
   current-gene-sample-data,
   additional-gene-sample-data,
   duplicates,
   index-mapping,
   append-only?]
  (reduce-kv
    (fn [sample-data-map, old-sample-index, sample-data]
      (let [new-sample-index (get index-mapping old-sample-index)]
        (if (contains? duplicates old-sample-index)
          ; store data under new sample id
          (update sample-data-map new-sample-index combine-data sample-data, append-only?)
          (assoc sample-data-map new-sample-index sample-data))))
    (get current-gene-sample-data sample-data-attribute)
    (get additional-gene-sample-data sample-data-attribute)))


(defn merge-alterations-append-or-overwrite
  [current-sample-count, additional-sample-count, current-alteration-map, additional-alteration-map, duplicates, index-mapping, append-only?]
  (let [current-alterations (:alterations current-alteration-map),
        additional-alterations (into (im/dense-int-set)
                                 (map index-mapping)
                                 (:alterations additional-alteration-map))
        duplicates (into #{} (map index-mapping) duplicates)
        current-missing-values (:missing-values current-alteration-map),
        additional-missing-values (into (im/dense-int-set)
                                    (map index-mapping)
                                    (:missing-values additional-alteration-map))
        total-sample-count (+ current-sample-count additional-sample-count)]
    {:alterations (or
                    (if additional-alteration-map
                      (if current-alteration-map
                        ; data in both: CURRENT and ADDITIONAL dataset
                        (if append-only?
                          ; just append
                          (into current-alterations (remove duplicates) additional-alterations)
                          ; overwrite
                          (-> (im/dense-int-set)
                            ; remove duplicates from CURRENT data
                            (into (remove duplicates) current-alterations)
                            (im/union additional-alterations)))
                        ; no data for the given (implicit) alteration-type of the CURRENT dataset
                        additional-alterations)
                      ; no data for the given (implicit) alteration-type of the ADDITIONAL dataset
                      current-alterations)
                    ; empty set as default
                    (im/dense-int-set))

     :missing-values (or
                       (if additional-alteration-map
                         (if current-alteration-map
                           ; data in both: CURRENT and ADDITIONAL dataset
                           (if append-only?
                             ; just append
                             (into current-missing-values (remove duplicates) additional-missing-values)
                             ; overwrite
                             (-> (im/dense-int-set)
                               ; remove duplicates from CURRENT missing values
                               (into (remove duplicates) current-missing-values)
                               (im/union additional-missing-values)))
                           ; no data for the given (implicit) alteration-type of the CURRENT dataset
                           ; mark old samples as missing values
                           (into additional-missing-values (range current-sample-count)))
                         ; no data for the given (implicit) alteration-type of the ADDITIONAL dataset
                         ; mark new samples as missing values
                         (into current-missing-values (range current-sample-count total-sample-count)))
                       ; complete set as default
                       (im/dense-int-set (range total-sample-count)))}))


(defn merge-gene-data-append-or-overwrite
  [merged-alteration-types, current-sample-count, additional-sample-count, gene-name, current-gene-data, additional-gene-data, duplicates, index-mapping, append-only?]
  {:gene gene-name,
   :group (or (:group current-gene-data) (:group additional-gene-data)),
   :order-fixed? (boolean (:order-fixed? current-gene-data))
   :alteration-data (reduce
                      (fn [result-map, alteration-type]
                        (assoc result-map
                          alteration-type
                          (merge-alterations-append-or-overwrite
                            current-sample-count,
                            additional-sample-count,
                            (get-in current-gene-data [:alteration-data, alteration-type]),
                            (get-in additional-gene-data [:alteration-data, alteration-type])
                            duplicates
                            index-mapping
                            append-only?)))
                      {}
                      merged-alteration-types)})




(defn merge-gene-sample-data-append-or-overwrite
  "Merges the given gene-sample data onto the existing overwriting previously attribute values."
  [current-gene-sample-data, additional-gene-sample-data, append-only?]
  (let [{current-gene-list :gene-list,
         current-sample-ids :sample-id-vec,
         current-sample-count :sample-count} current-gene-sample-data,
        {additional-gene-list :gene-list,
         additional-sample-ids :sample-id-vec,
         additional-sample-count :sample-count} additional-gene-sample-data,
        {:keys [sample-ids, duplicates, index-mapping]} (sample-id-merge current-sample-ids, additional-sample-ids)
        clinical-data-map (merge-sample-data-maps-without-duplicates
                            :clinical-data-map, current-gene-sample-data, additional-gene-sample-data, duplicates, index-mapping, append-only?)
        merged-alteration-types (into (:alteration-type-set current-gene-sample-data) (:alteration-type-set additional-gene-sample-data))]
    {:sample-count (count sample-ids)

     :sample-permutation (merge-sample-permutations-without-duplicates
                           (:sample-permutation current-gene-sample-data)
                           (:sample-permutation additional-gene-sample-data)
                           duplicates
                           index-mapping)

     :sample-group-map (merge-sample-data-maps-without-duplicates
                         :sample-group-map, current-gene-sample-data, additional-gene-sample-data, duplicates, index-mapping, append-only?)

     :sample-id-vec sample-ids

     :clinical-attributes (imp/clinical-attributes clinical-data-map)
     :clinical-data-map clinical-data-map

     :alteration-type-set merged-alteration-types

     :gene-list (let [current-gene-map (zipmap (mapv :gene current-gene-list) current-gene-list),
                      additional-gene-map (zipmap (mapv :gene additional-gene-list) additional-gene-list),
                      all-genes (->> (concat current-gene-list additional-gene-list)
                                  (mapv :gene)
                                  distinct
                                  vec)]
                  (reassign-gene-ids
                    (mapv
                      (fn [gene-name]
                        (let [current-gene-data (get current-gene-map gene-name),
                              additional-gene-data (get additional-gene-map gene-name)]
                          (merge-gene-data-append-or-overwrite
                            merged-alteration-types, current-sample-count, additional-sample-count, gene-name,
                            current-gene-data, additional-gene-data, duplicates, index-mapping, append-only?)))
                      all-genes)))}))


(defn insert-gene-sample-data-append-or-overwrite
    ([data-mgmt, additional-gene-sample-data, append-only?]
     (dm/modify-current-data data-mgmt, merge-gene-sample-data-append-or-overwrite, additional-gene-sample-data, append-only?))
    ([data-mgmt, additional-gene-sample-data, append-only?, sample-group]
     (insert-gene-sample-data-append-or-overwrite data-mgmt,
       (assign-sample-group additional-gene-sample-data, sample-group)
       append-only?)))


(defn same-sample-set?
  "Checks whether the given gene-sample-data has exactly the same sample ids as the current one."
  [data-mgmt, additional-gene-sample-data]
  (let [gene-sample-data (dm/current-data data-mgmt)]
    (=
      (-> gene-sample-data :sample-id-vec set)
      (-> additional-gene-sample-data :sample-id-vec set))))


(defn index-renaming-via-id
  [current-sample-ids, additional-sample-ids]
  (let [current-sample-id->index (zipmap current-sample-ids (range))
        additional-sample-index->id (zipmap (range) additional-sample-ids)]
    (persistent!
      (u/reduce-indexed
        (fn [renaming-map, additional-index, sample-id]
          (let [current-index (get current-sample-id->index sample-id)]
            (assoc! renaming-map additional-index current-index)))
        (transient {})
        additional-sample-ids))))


(defn rename-set
  [renaming-map, set]
  (persistent!
    (reduce
      (fn [result-set, element]
        (conj! result-set (get renaming-map element)))
      (transient (im/dense-int-set))
      set)))


(defn merge-alterations-merge-samples
  [additional-sample-index->current-sample-index, current-alteration-map, additional-alteration-map]
  {:alterations (if current-alteration-map
                  (im/union
                    (:alterations current-alteration-map),
                    (rename-set
                      additional-sample-index->current-sample-index,
                      (:alterations additional-alteration-map)))
                  (rename-set
                    additional-sample-index->current-sample-index,
                    (:alterations additional-alteration-map))),
   :missing-values (if current-alteration-map
                     (if additional-alteration-map
                       (im/intersection
                         (:missing-values current-alteration-map),
                         (rename-set
                           additional-sample-index->current-sample-index,
                           (:missing-values additional-alteration-map)))
                       (:missing-values current-alteration-map))
                     (if additional-alteration-map
                       (rename-set
                         additional-sample-index->current-sample-index,
                         (:missing-values additional-alteration-map))
                       (im/dense-int-set)))})


(defn merge-gene-data-merge-samples
  [merged-alteration-types, additional-sample-index->current-sample-index, gene-name, current-gene-data, additional-gene-data]
  {:gene gene-name,
   :group (or (:group current-gene-data) (:group additional-gene-data)),
   :order-fixed? (boolean (:order-fixed? current-gene-data))
   :alteration-data (reduce
                      (fn [result-map, alteration-type]
                        (assoc result-map
                          alteration-type
                          (merge-alterations-merge-samples
                            additional-sample-index->current-sample-index,
                            (get-in current-gene-data [:alteration-data, alteration-type]),
                            (get-in additional-gene-data [:alteration-data, alteration-type]))))
                      {}
                      merged-alteration-types)})


(defn merge-clinical-data
  [additional-sample-index->current-sample-index, current-clinical-data-map, additional-clinical-data-map]
  (persistent!
    (reduce-kv
      (fn [result-map, additional-sample-index, clinical-data]
        (let [current-sample-index (get additional-sample-index->current-sample-index additional-sample-index)]
          ; merge additional clinical data for the sample to data for the current sample index
          (assoc! result-map
            current-sample-index
            (merge
              (get current-clinical-data-map current-sample-index {})
              clinical-data))))
      (transient (or current-clinical-data-map {}))
      additional-clinical-data-map)))


(defn merge-data-merge-samples
  "Merges the two gene-sample-data instances by merging the data of the samples (assuming that both instances have exactly the same samples)."
  [current-gene-sample-data, additional-gene-sample-data]
  (let [{current-gene-list :gene-list,
         current-sample-ids :sample-id-vec,
         current-clinical-data-map :clinical-data-map,
         :keys [sample-count,
                sample-permutation,
                sample-group-map]} current-gene-sample-data,
        {additional-gene-list :gene-list,
         additional-sample-ids :sample-id-vec,
         additional-clinical-data-map :clinical-data-map,} additional-gene-sample-data,
        additional-sample-index->current-sample-index (index-renaming-via-id current-sample-ids, additional-sample-ids),
        merged-alteration-types (into (set (:alteration-type-set current-gene-sample-data)) (:alteration-type-set additional-gene-sample-data)),
        clinical-data-map (merge-clinical-data additional-sample-index->current-sample-index,
                            current-clinical-data-map,
                            additional-clinical-data-map)]
    {:sample-count sample-count
     :sample-permutation sample-permutation
     :sample-group-map sample-group-map,
     :sample-id-vec current-sample-ids,
     :clinical-attributes (imp/clinical-attributes clinical-data-map),
     :clinical-data-map clinical-data-map
     :alteration-type-set merged-alteration-types,
     :gene-list (let [current-gene-map (zipmap (mapv :gene current-gene-list) current-gene-list),
                      additional-gene-map (zipmap (mapv :gene additional-gene-list) additional-gene-list),
                      all-genes (->> (concat current-gene-list additional-gene-list)
                                  (mapv :gene)
                                  distinct
                                  vec)]
                  (reassign-gene-ids
                    (mapv
                      (fn [gene-name]
                        (let [current-gene-data (get current-gene-map gene-name),
                              additional-gene-data (get additional-gene-map gene-name)]
                          (merge-gene-data-merge-samples merged-alteration-types, additional-sample-index->current-sample-index, gene-name, current-gene-data, additional-gene-data)))
                      all-genes)))}))


(defn insert-gene-sample-data-merge-samples
  [data-mgmt, additional-gene-sample-data]
  (dm/modify-current-data data-mgmt, merge-data-merge-samples, additional-gene-sample-data))


(defn gene-group-member-indices
  "Returns the indices of the genes that are in the same groups as the given genes."
  [{:keys [gene-list] :as gene-sample-data}, gene-indices]
  ; use keep to ignore nils signaling "no group"
  (let [group-set (->> gene-indices (keep #(:group (nth gene-list %))) set)]
    (u/reduce-indexed
      (fn [member-indices, index, {:keys [group] :as gene}]
        (cond-> member-indices
          (contains? group-set group)
          (conj index)))
      []
      gene-list)))


(defn sample-group-member-indices
  [{:keys [sample-permutation, sample-group-map] :as gene-sample-data}, sample-column-indices]
  (let [{:keys [column->sample, sample->column]} sample-permutation,
        sample-indices (mapv column->sample sample-column-indices),
        ; if there are no groups, use an empty map
        sample-group-map (or sample-group-map {}),
        ; include nil groups since it counts as implicit group of "all other samples not in a named group"
        sample-groups (->> sample-indices (map sample-group-map) set)]
    (reduce-kv
      (fn [member-indices, sample, column]
        (cond-> member-indices
          (contains? sample-groups (get sample-group-map sample))
          ; add column of sample
          (conj column)))
      []
      sample->column)))


(defn assign-group-by-gene-id
  [data-mgmt, group-name, group-gene-ids]
  (let [gene-id-set (set group-gene-ids)]
    (dm/modify-current-data data-mgmt,
      (fn [{:keys [gene-list] :as gene-sample-data}]
        (assoc gene-sample-data
          :gene-list
          (mapv
            (fn [gene]
              (cond-> gene
                (contains? gene-id-set (:gene-id gene))
                (assoc :group group-name)))
            gene-list))))))



(defn compare-ignore-case
  [a, b]
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) 1
    (nil? b) -1
    :else (let [res (compare (str/lower-case a), (str/lower-case b))]
            (if (zero? res)
              ; two groups or genes are only considered equal if they are case sensitive equal
              (compare a b)
              res))))


(defn do-sort-genes-by-group
  [gene-list]
  (let [{fixed-genes true, non-fixed-genes false} (group-by (comp boolean :order-fixed?) gene-list)]
    (into
      (vec fixed-genes)
      (sort
        (fn [{group-1 :group, gene-name-1 :gene, :as gene-1}, {group-2 :group, gene-name-2 :gene, :as gene-2}]
          ; compare groups
          (let [res (compare-ignore-case group-1, group-2)]
            (if (zero? res)
              ; result by gene name comparison
              (compare-ignore-case gene-name-1, gene-name-2)
              ; result from group comparison
              res)))
        non-fixed-genes))))


(defn sort-genes-by-group
  [data-mgmt, alteration-type]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (col-sort/lexicographic-sort true, false, alteration-type, (update-in gene-sample-data [:gene-list] do-sort-genes-by-group)))))


(defn do-sort-genes-by-alteration-count
  [gene-list, altertation-type]
  (let [{fixed-genes true, non-fixed-genes false} (group-by (comp boolean :order-fixed?) gene-list)]
    (into
      (vec fixed-genes)
      (->> non-fixed-genes
        (mapv (fn [gene] (assoc gene ::alteration-count (-> gene (get-in [:alteration-data, altertation-type, :alterations]) count))))
        (sort
          (fn [{group-1 :group, gene-name-1 :gene, :as gene-1}, {group-2 :group, gene-name-2 :gene, :as gene-2}]
            (let [coverage-res (compare (::alteration-count gene-1) (::alteration-count gene-2))]
              (if (zero? coverage-res)
                (let [group-res (compare-ignore-case group-1, group-2)]
                  (if (zero? group-res)
                    (compare-ignore-case gene-name-1, gene-name-2)
                    group-res))
                ; sort decreasingly by coverage
                (- coverage-res)))))
        (mapv #(dissoc % ::alteration-count))))))


(defn on-sort-genes-by-alteration-count
  [data-mgmt, alteration-type]
  (dm/modify-current-data data-mgmt,
    (fn [gene-sample-data]
      (col-sort/lexicographic-sort true, false, alteration-type,
        (update-in gene-sample-data [:gene-list] do-sort-genes-by-alteration-count alteration-type)))))


(defn do-merge-group
  [gene-list, group-genes, result-gene]
  (let [group-genes-set (set group-genes)
        insert-pos (u/some-index group-genes-set gene-list)]
    (persistent!
      (u/reduce-indexed
        (fn [new-gene-list, index, gene]
          (cond-> new-gene-list
            (not (contains? group-genes-set gene)) (conj! gene)
            (= index insert-pos) (conj! result-gene)))
        (transient [])
        gene-list))))


(defn merge-group
  [data-mgmt, group-genes, result-gene]
  (dm/modify-current-data data-mgmt
    (fn [gene-sample-data]
      (update-in gene-sample-data [:gene-list] do-merge-group group-genes, result-gene))))


(defn sample-group-map-by-clinical-attribute
  [clinical-data-map, attribute]
  (persistent!
    (reduce-kv
      (fn [sample-group-map, sample-index, sample-data]
        (assoc! sample-group-map sample-index (u/trim-nil (get sample-data attribute))))
      (transient {})
      clinical-data-map)))


(defn assign-sample-groups-by-clinical-attribute
  "Assigns sample groups according to the chosen clinical attribute.
  The alteration-type is used for sample sorting within the groups."
  [data-mgmt, attribute, alteration-type]
  (dm/modify-current-data data-mgmt
    (fn [{:keys [clinical-data-map] :as gene-sample-data}]
      (col-sort/lexicographic-sort true, false, alteration-type,
        (assoc gene-sample-data :sample-group-map (sample-group-map-by-clinical-attribute clinical-data-map, attribute))))))


(defn rename-chosen-genes
  [data-mgmt, renaming-map]
  (dm/modify-current-data data-mgmt
    (fn [{:keys [gene-list] :as gene-sample-data}]
      (assoc gene-sample-data
        :gene-list
        (mapv
          (fn [{:keys [gene] :as gene-data}]
            (if-let [new-name (get renaming-map gene)]
              (assoc gene-data :gene new-name)
              gene-data))
          gene-list)))))


(def ^:const no-group "NO GROUP")

(defn sample-group->indices
  [{:keys [sample-group-map],
    {:keys [column->sample]} :sample-permutation,
    :as gene-sample-data}]
  (persistent!
    (reduce
      (fn [result-map, sample-index]
        (let [group (or (get sample-group-map sample-index) no-group)]
          (u/update-in! result-map [group] (fnil conj (im/dense-int-set)) sample-index)))
      (transient {})
      column->sample)))


(defn sample-group-order
  [{:keys [sample-group-map],
    {:keys [column->sample]} :sample-permutation,
    :as gene-sample-data}]
  (into []
    (comp
      ; groups
      (map #(or (get sample-group-map %) no-group))
      ; each group only once
      (distinct))
    column->sample))


(defn gene-group
  [{:keys [group]}]
  (or group no-group))


(defn gene-group-order
  [{:keys [gene-list], :as gene-sample-data}]
  (into []
    (comp
      ; group
      (map gene-group)
      ; each group only once
      (distinct))
    gene-list))


(defn superfluous-no-group?
  [group-map]
  (and
    (= (count group-map) 2)
    (contains? group-map no-group)
    (->> group-map vals (apply =))))


(defn collect-sample-group-data
  [{:keys [sample-count], :as gene-sample-data}]
  (let [sample-group->indices (-> (sample-group->indices gene-sample-data)
                                (assoc "ALL" (reduce conj (im/dense-int-set) (range sample-count)))),
        sample-group-order (-> (sample-group-order gene-sample-data)
                             (conj "ALL"))]
    (if (superfluous-no-group? sample-group->indices)
      {:sample-group->indices (dissoc sample-group->indices no-group),
       :sample-group-order (u/removev #(= % no-group) sample-group-order)}
      {:sample-group->indices sample-group->indices,
       :sample-group-order sample-group-order})))


(defn objective-per-groups
  "Calculates objectives (coverage and overlap) per group.
  Relative values are calculated relative to the sample group size."
  [{:keys [gene-list, sample-count], :as gene-sample-data}]
  (let [{:keys [sample-group->indices, sample-group-order]} (collect-sample-group-data gene-sample-data),
        group->genes (-> (group-by gene-group gene-list)
                       (assoc "ALL" gene-list)),
        gene-group-order (-> (gene-group-order gene-sample-data)
                           (conj "ALL")),
        [group->genes, gene-group-order] (if (superfluous-no-group? group->genes)
                                           [(dissoc group->genes no-group),
                                            (u/removev #(= % no-group) gene-group-order)]
                                           [group->genes, gene-group-order])]
    {:sample-groups sample-group-order,
     :gene-groups gene-group-order,
     :objective-per-groups (into {}
                             (for [[sample-group, sample-index-set] sample-group->indices,
                                   [gene-group, gene-list] group->genes]
                               (vector
                                 [gene-group, sample-group]
                                 (let [objective-map (score/total-overlap+coverage
                                                       :alterations,
                                                       (co/limit-alterations-to-sample-set sample-count, sample-index-set, gene-list))]
                                   {:absolute objective-map,
                                    :relative (u/normalize-values (count sample-index-set), objective-map)}))))}))


(defn alterations-per-sample-group-and-gene
  "Calculates absolute number of alterations and the relative ratio of alterations within the group."
  [{:keys [gene-list, sample-count], :as gene-sample-data}]
  (let [{:keys [sample-group->indices, sample-group-order]} (collect-sample-group-data gene-sample-data)]
    {:sample-groups sample-group-order,
     :gene-names (mapv :gene gene-list),
     :grouped-alteration-data (into {}
                                (for [[sample-group, sample-index-set] sample-group->indices,
                                      :let [sample-group-count (count sample-index-set),
                                            gene-list (co/limit-alterations-to-sample-set sample-count, sample-index-set, gene-list)]
                                      gene-data gene-list]
                                  (vector
                                    [(:gene gene-data), sample-group]
                                    (let [alterations-count (count (:alterations gene-data))]
                                      {:absolute alterations-count,
                                       :relative (/ (double alterations-count) sample-group-count)}))))}))


(defn clinical-attribute-values-frequencies-per-sample-group
  [{:keys [clinical-data-map, sample-count] :as gene-sample-data}, selected-clinical-attribute]
  (let [{:keys [sample-group->indices, sample-group-order]} (collect-sample-group-data gene-sample-data),
        attribute-values-map (persistent!
                               (reduce-kv
                                 (fn [attribute-values-map, sample-id, attribute-map]
                                   (assoc! attribute-values-map sample-id (co/attribute-value attribute-map, selected-clinical-attribute)))
                                 (transient {})
                                 clinical-data-map)),
        attribute-values (-> (->> attribute-values-map vals distinct sort vec)
                           (conj "ALL")),
        clinical-attribute-values (reduce-kv
                                    (fn [clinical-attribute-values, sample-group, sample-index-set]
                                      (let [sample-group-count (count sample-index-set),
                                            group-attribute-values-map (select-keys attribute-values-map sample-index-set),
                                            attribute-freq-map (->> group-attribute-values-map vals frequencies)]
                                        (assoc clinical-attribute-values
                                          sample-group
                                          {:absolute (assoc attribute-freq-map
                                                       "ALL" sample-group-count),
                                           :relative (assoc (u/normalize-values sample-count, attribute-freq-map)
                                                       "ALL" (/ (double sample-group-count) sample-count))})))
                                    {}
                                    sample-group->indices)]
    {:sample-groups sample-group-order,
     :clinical-attribute-values attribute-values,
     :grouped-clinical-attribute-data clinical-attribute-values}))



(defn add-clinical-data
  [data-mgmt, {:keys [clinical-attributes, clinical-data-map] :as clinical-data}]
  (dm/modify-current-data data-mgmt,
    (fn [{:keys [sample-id-vec] :as gene-sample-data}]
      (let [clinical-data-map (persistent!
                                (u/reduce-indexed
                                  ; transform ID based map to INDEX based map
                                  (fn [result-map, index, sample-id]
                                    (cond-> result-map
                                      sample-id
                                      (assoc! index (get clinical-data-map sample-id))))
                                  (transient {})
                                  sample-id-vec))]
        (-> gene-sample-data
          (update-in [:clinical-attributes] into clinical-attributes)
          (update-in [:clinical-data-map] u/deep-merge-non-delete clinical-data-map))))))


(defn rename-sample-groups
  [data-mgmt, group-renaming-map]
  (dm/modify-current-data data-mgmt,
    (fn [{:keys [sample-group-map] :as gene-sample-data}]
      (assoc gene-sample-data
        :sample-group-map
        (persistent!
          (reduce-kv
            (fn [result-map, sample-id, sample-group]
              (if (contains? group-renaming-map sample-group)
                (assoc! result-map sample-id (get group-renaming-map sample-group))
                result-map))
            (transient sample-group-map)
            sample-group-map))))))


(defn rename-gene-groups
  [data-mgmt, group-renaming-map]
  (dm/modify-current-data data-mgmt,
    (fn [{:keys [gene-list] :as gene-sample-data}]
      (assoc gene-sample-data
        :gene-list
        (mapv
          (fn [{:keys [group] :as gene-data}]
            (if (contains? group-renaming-map group)
              (assoc gene-data :group (get group-renaming-map group))
              gene-data))
          gene-list)))))


(defn rename-in-clinical-attributes-set
  [attribute-renaming, clinical-attributes]
  (persistent!
    (reduce-kv
      (fn [result-set, new-attribute, old-attributes]
        (cond-> (reduce disj! result-set old-attributes)
          new-attribute
          (conj! new-attribute)))
      (transient clinical-attributes)
      (u/inverse-map attribute-renaming))))


(defn rename-clincal-attributes
  [attribute-renaming, sample-data]
  ; first reduce renaming-map to present attributes
  (let [attribute-renaming (select-keys attribute-renaming (keys sample-data))]
    (persistent!
      (reduce-kv
        (fn [result-map, dst-attribute, src-attributes]
          (let [result-map (reduce dissoc! result-map src-attributes)]
            (if dst-attribute
              (let [value-list (u/keepv
                                 #(get sample-data %)
                                 (cond-> src-attributes
                                   (contains? sample-data dst-attribute)
                                   (conj dst-attribute)))]
                (assert (<= (count value-list) 1))
                (assoc! result-map dst-attribute (first value-list)))
              result-map)))
        (transient sample-data)
        (u/inverse-map attribute-renaming)))))


(defn rename-clincal-values
  [value-renaming-per-attribute, sample-data]
  ; first reduce renaming-map to present attributes
  (let [value-renaming-per-attribute (select-keys value-renaming-per-attribute (keys sample-data))]
    (persistent!
      (reduce-kv
        (fn [result-map, attribute, value-renaming]
          (let [value (get sample-data attribute)]
            ; keep values that are not specified in the renaming
            (cond-> result-map
              (contains? value-renaming value)
              (assoc! attribute (get value-renaming value)))))
        (transient sample-data)
        value-renaming-per-attribute))))


(defn rename-clinical-attributes-and-values
  [data-mgmt, attribute-renaming, value-renaming-per-attribute]
  (dm/modify-current-data data-mgmt,
    (fn [{:keys [clinical-attributes, clinical-data-map] :as gene-sample-data}]
      (assoc gene-sample-data
        :clinical-attributes (rename-in-clinical-attributes-set attribute-renaming, clinical-attributes)
        :clinical-data-map (persistent!
                             (reduce-kv
                               (fn [result-map, sample-index, sample-data]
                                 (assoc! result-map sample-index
                                   (some->> sample-data
                                     (rename-clincal-attributes attribute-renaming)
                                     (rename-clincal-values value-renaming-per-attribute))))
                               (transient clinical-data-map)
                               clinical-data-map))))))


(defn export-clinical-data
  [data-mgmt, file]
  (let [{:keys [clinical-attributes, clinical-data-map, sample-id-vec] :as data} (dm/current-data data-mgmt)
        clinical-attributes (sort clinical-attributes)
        lines (persistent!
                (reduce-kv
                  (fn [line-vec, sample-index, attribute-map]
                    (let [sample-id (nth sample-id-vec sample-index sample-index)]
                      (conj! line-vec
                        (list* sample-id
                          (mapv
                            (fn [attribute]
                              (get attribute-map attribute ""))
                            clinical-attributes)))))
                  (transient [])
                  clinical-data-map))
        sep (u/column-separator file)
        file-content (->> lines
                       (list* (list* "SAMPLE ID" clinical-attributes))
                       (mapv #(str/join sep, (mapv u/quoted-str %)))
                       (str/join "\n"))]
    (spit file, file-content)))


(defn problematic-duplicate-sample-ids
  "When one of the imported alteration types is already contained in the current data,
  determine the subset of the new sample ids that is already contained in the current data (if any)."
  [data-mgmt, new-gene-sample-data]
  (let [{new-sample-ids :sample-id-vec
         new-alteration-types :alteration-type-set} new-gene-sample-data
        {current-sample-ids :sample-id-vec
         current-alteration-types :alteration-type-set} (dm/current-data data-mgmt)]
    (when (seq (u/intersection-fast current-alteration-types, new-alteration-types))
      (let [duplicate-set (u/intersection-fast (set current-sample-ids), (set new-sample-ids))]
        (not-empty duplicate-set)))))
