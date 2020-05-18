; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.solution-comparison-view
  (:require [clj-jfx.core :as jfx]
            [avatar.data.management :as dm]
            [clj-jfx.properties :as props]
            [clj-jfx.treetable :as treetable]
            [clj-jfx.table :as table]
            [avatar.util :as u]
            [avatar.data.gene-data :as gd]
            [clojure.string :as str]
            [clj-jfx.selection :as jfx-select]
            [clojure.set :as set]
            [avatar.ui.dialogs.progress-view :as progress]
            [avatar.ui.solution-comparison-export :as sce]
            [clj-jfx.font-selection :as font-sel]
            [clj-jfx.colorchooser :as cc]
            [avatar.ui.tools :as t])
  (:import (javafx.scene.control Label TextField TableCell TableColumn)
           (javafx.scene.paint Color)
           (clj_jfx.table CustomTableCell)))



(defn gene-set
  [{:keys [gene-list]}]
  (into #{} (comp (remove gd/fixed?) (map :gene)) gene-list))


(defn snapshot-item
  [id-fn, {:keys [name, data] :as snapshot}]
  ; no coverage overlap, since it
  {:properties {:name name
                :id (id-fn)
                :snapshot name,
                :checked? false
                :gene-count (gd/gene-count data)
                :sample-count (gd/sample-count data)
                :gene-set (gene-set data)
                :gene-sample-data data
                :type :snapshot-item}})


(defn solution-item
  [id-fn, snapshot, optimization-sample-group, name, {:keys [relative-coverage, relative-overlap, plot-data] :as solution}]
  {:properties {:name name
                :id (id-fn)
                :snapshot snapshot
                :checked? false
                :gene-count (gd/gene-count plot-data)
                :sample-count (gd/sample-count plot-data)
                :optimization-coverage relative-coverage
                :optimization-overlap relative-overlap
                :optimization-sample-group optimization-sample-group
                :gene-set (gene-set plot-data)
                :gene-sample-data plot-data
                :type :solution-item}})


(defn saved-pareto-front-item
  [id-fn, {:keys [snapshot, name, data] :as saved-pareto-front}]
  {:properties {:name name
                :checked? false}
   :children (u/mapv-indexed
               (fn [index, solution]
                 (solution-item id-fn, snapshot, (get-in data [:parameters, :sample-group]), (str name " #" (inc index)), solution))
               (sort-by :relative-coverage >
                 (:pareto-front data)))})


(defn batch-pareto-front-item
  [id-fn, batch-name, pareto-front-index, {:keys [snapshot, name, data] :as pareto-front}]
  {:properties {:name (str pareto-front-index " " (str/join " / " name))
                :checked? false}
   :children (u/mapv-indexed
               (fn [index, solution]
                 (solution-item id-fn, snapshot, (get-in data [:parameters, :sample-group]), (str batch-name "/" pareto-front-index " #" (inc index)), solution))
               (sort-by :relative-coverage >
                 (:pareto-front data)))})

(defn batch-item
  [id-fn, {:keys [name, pareto-front-list] :as batch}]
  {:properties {:name name,
                :checked? false},
   :children (u/mapv-indexed
               (fn [index, pareto-front]
                 (batch-pareto-front-item id-fn, name, (inc index), pareto-front))
               pareto-front-list)})


(defn solution-selection-tree-data
  [ui]
  (let [data-mgmt (:data-management ui)
        solution-id (volatile! 0)
        id-fn (fn [] (vswap! solution-id inc))]
    {:properties {:name "ROOT"}
     :children (vector
                 {:properties {:name "Snapshots"
                               :checked? false}
                  :children (mapv (partial snapshot-item id-fn) (dm/snapshots data-mgmt))}
                 {:properties {:name "Saved Pareto Fronts"
                               :checked? false}
                  :children (mapv (partial saved-pareto-front-item id-fn) (dm/saved-pareto-fronts data-mgmt))}
                 {:properties {:name "Batches"
                               :checked? false}
                  :children (mapv (partial batch-item id-fn) (dm/pareto-front-batches data-mgmt))})}))


(defn table-content
  [color, value]
  (when value
    {:color (cond
              (= value "no") nil
              (= value "yes") color)
     :text (when-not (contains? #{"yes" "no"} value)
             value)}))


(defn solution-id-predicate
  [id-set]
  (fn [{:keys [id]}]
    (contains? id-set id)))


(defn unbind-if-needed
  [cell, prop, observable-atom]
  (when-let [observable (deref observable-atom)]
    (props/unbind (props/property cell, prop), observable)
    (reset! observable-atom nil))
  nil)

(defn gene-membership-cell-factory
  [settings-map-prop, _]
  (let [cell (table/table-cell)
        content-observable (table/cell-observable-value-property cell)
        empty?-prop (props/property cell, :empty)
        background-prop (props/fn-property
                          (fn [settings-map, content, {column-id :id, :as column-data}, empty?]
                            (when (and (not empty?) column-id content (= content "yes"))
                              (when-let [color (get-in settings-map [:solutions, column-id, :color])]
                                (jfx/background {:fills [{:color color, :insets 1.0}]}))))
                          settings-map-prop
                          content-observable
                          (table/column-data-property cell)
                          empty?-prop)
        text-prop (props/fn-property
                    (fn [content, empty?]
                      (when-not (or empty? (contains? #{"yes" "no"} content))
                        content))
                    content-observable
                    empty?-prop)
        font-prop (props/fn-property (comp jfx/font :objective-value-font) settings-map-prop)]
    (props/bind (props/property cell, :background) background-prop)
    (props/bind (props/property cell, :text), text-prop)
    (props/bind (props/property cell, :font), font-prop)
    cell))


(defn gene-column-cell-factoy
  [settings-map-prop, rows-prop, _]
  (let [prev-font-observable-atom (atom nil)
        prev-content-observable-atom (atom nil)]
    (table/table-cell
      (fn [^TableCell cell, item, empty?]
        (if empty?
          (do
            (unbind-if-needed cell, :font, prev-font-observable-atom)
            (unbind-if-needed cell, :text, prev-content-observable-atom)
            (reset! prev-content-observable-atom nil)
            (.setText cell nil))
          (let [content-observable (-> cell .getTableColumn (.getCellObservableValue (.getIndex cell)))
                prev-content-observable (deref prev-content-observable-atom)]
            (when-not (= content-observable prev-content-observable)
              (unbind-if-needed cell, :font, prev-font-observable-atom)
              (unbind-if-needed cell, :text, prev-content-observable-atom)
              (reset! prev-content-observable-atom content-observable)
              (let [row-index (.getIndex (.getTableRow cell))
                    font-prop (props/fn-property
                                (fn [{:keys [gene-font, objective-label-font, objective-rows]}, row-count]
                                  (jfx/font
                                    (if (< row-index (- row-count (count objective-rows)))
                                      gene-font
                                      objective-label-font)))
                                settings-map-prop
                                (props/fn-property
                                  count
                                  rows-prop))]
                (props/bind (props/property cell, :text), content-observable)
                (props/bind (props/property cell, :font), font-prop)
                (jfx/alignment! cell, :center-left)
                (reset! prev-font-observable-atom font-prop)))))))))


(defn special-column-id?
  [id]
  (contains? #{:genes-column, :coverage-column} id))


(defn special-column?
  [{:keys [id] :as column-spec}]
  (special-column-id? id))


(defn update-gene-table-data
  [gene-cell-factory, coverage-cell-factory, #_solution-click-handler,
   {old-rows :rows, old-columns :columns}
   [old-settings-map, old-selected-solutions]
   [new-settings-map, new-selected-solutions]]
  (let [new-ids (into #{} (map :id) new-selected-solutions)
        old-ids (into #{} (map :id) old-selected-solutions)
        added-ids (u/set-difference new-ids, old-ids)
        {:keys [rotate-column-captions?
                solution-column-width
                selected-snapshot
                selected-alteration-type
                gene-column-label
                gene-column-width
                gene-column-label-font
                coverage-label
                coverage-label-font
                coverage-column-width
                solution-caption-font]} new-settings-map
        remaining-columns (into []
                            (comp
                              (filter
                                (fn [{:keys [id]}]
                                  (or (= id :genes-column) (contains? new-ids id))))
                              (map
                                (fn [{:keys [name, id] :as column}]
                                  (assoc column
                                    :name (if (= id :genes-column)
                                            gene-column-label
                                            (or (get-in new-settings-map [:solutions, id, :caption]) name))
                                    :font (if (= id :genes-column)
                                            gene-column-label-font
                                            solution-caption-font)
                                    :vertical? rotate-column-captions?
                                    :width (if (= id :genes-column)
                                             gene-column-width
                                             solution-column-width)))))
                            old-columns)
        additional-solutions (filterv (solution-id-predicate added-ids) new-selected-solutions)
        additional-columns (mapv
                             (fn [{:keys [name, id]}]
                               {:name (or (get-in new-settings-map [:solutions, id, :caption]) name)
                                :id id
                                :attribute id
                                :vertical? rotate-column-captions?
                                :width solution-column-width
                                :font solution-caption-font
                                :sortable? false
                                :reorderable? true
                                ;:click-handler (fn [c] (solution-click-handler id, c))
                                :cell-factory gene-cell-factory})
                             additional-solutions)
        ; coverage column ?
        coverage-column? (and selected-snapshot (not= selected-snapshot :coverage-snapshot/none))
        gene->coverage-map (when coverage-column?
                             (gd/gene-coverage-in-snapshot selected-alteration-type, selected-snapshot))
        ; new columns
        new-columns (cond-> (into remaining-columns additional-columns)
                      coverage-column?
                      (conj {:name coverage-label
                             :id :coverage-column
                             :attribute :coverage
                             :vertical? (if (nil? rotate-column-captions?) true rotate-column-captions?)
                             :font coverage-label-font
                             :width coverage-column-width
                             :sortable? false
                             :cell-factory coverage-cell-factory}))

        ; determine solution order based on columns
        new-gene-set (into #{} (comp (map :gene-set) cat) new-selected-solutions)
        previous-gene-order (mapv :gene-name old-rows)
        ; add new genes to the order (but keep order of previous genes)
        gene-order (into (filterv new-gene-set previous-gene-order)
                     (sort (u/set-difference new-gene-set (set previous-gene-order))))
        gene-rows (mapv
                    (fn [gene]
                      (persistent!
                        (reduce
                          (fn [row-map, {:keys [id, gene-set]}]
                            (assoc! row-map id (if (contains? gene-set gene) "yes" "no")))
                          (cond-> (transient {:gene-name gene, :type :gene-row})
                            coverage-column?
                            (assoc! :coverage
                              (let [coverage (get gene->coverage-map gene)]
                                (if (or (nil? coverage) (Double/isNaN coverage))
                                  "N/A"
                                  (format "%.1f%%" coverage)))))
                          new-selected-solutions)))
                    gene-order)
        objective-rows (mapv
                         (fn [objective-row]
                           (let [{:keys [objective-text]
                                  :as objectives-per-solution} (gd/objective-in-solution selected-alteration-type, new-selected-solutions, objective-row)]
                             (-> objectives-per-solution
                               (dissoc :objective-text)
                               (assoc
                                 :gene-name objective-text
                                 :type :objective-row))))
                         (:objective-rows new-settings-map))]
    {:columns new-columns
     :rows (into gene-rows objective-rows)}))


(defn label-cell
  [alignment]
  (let [label (Label.)]
    [(doto label (jfx/alignment! alignment)) (props/property label, :text)]))

(defn row-based-cell-factory
  [_]
  (-> (table/multi-functional-cell
        (fn determine-node+property [index, item]
          (let [[node, property] (case (int index)
                                   0 (let [edit (doto (TextField.)
                                                  (props/set-property :alignment (jfx/to-alignment :center)))]
                                       [edit, (props/property edit, :text)])
                                   1 (label-cell :center)
                                   2 (label-cell :center)
                                   3 (label-cell :center)
                                   4 (label-cell :center)
                                   5 (let [color-chooser (cc/color-chooser)] [color-chooser, (props/property color-chooser, :value)]))]
            {:node (jfx/max-size! node, Double/MAX_VALUE, Double/MAX_VALUE)
             :property property})))
    (jfx/padding! 0)))


(defn attributes-cell-factory
  [_]
  (let [label (doto (Label.)
                (jfx/max-size! Double/MAX_VALUE, Double/MAX_VALUE)
                (jfx/text-font! {:weight :bold}))]
    (CustomTableCell. label, (props/property label, :text))))


(defn formatted-objective
  [objective, default, solution]
  (if-let [value (get solution objective)]
    (format "%.4f" value)
    default))

(defn update-settings-table-data
  [settings-table-data-prop, selected-solutions, columns, settings-map]
  (let [solution-columns (u/removev special-column? columns)
        {old-columns :columns, old-rows :rows} (props/get settings-table-data-prop)
        old-colum-count (count old-columns)
        new-columns (into [{:name "Attributes"
                            :attribute 0
                            :id :attributes-column
                            :width 125
                            :sortable? false
                            :editable? false
                            :reorderable? false
                            :cell-factory attributes-cell-factory}]
                      (map-indexed
                        (fn [index, {:keys [id, name] :as col}]
                          (let [col-index (inc index)]
                            (assoc col
                              :name (or (get-in settings-map [:solutions, id, :caption]) name)
                              :attribute col-index
                              :width 125
                              :sortable? false
                              :editable? true
                              :reorderable? false
                              :cell-factory row-based-cell-factory))))
                      solution-columns)
        id->solution (zipmap (map :id selected-solutions) selected-solutions)
        sorted-solutions (mapv (comp id->solution :id) solution-columns)
        new-rows (vector
                   (into ["Caption"] (mapv
                                       (fn [{:keys [id, name]}]
                                         (or (get-in settings-map [:solutions, id, :caption]) name))
                                       sorted-solutions))
                   (into ["Name"] (mapv :name sorted-solutions))
                   (into ["Coverage"] (mapv (partial formatted-objective :optimization-coverage "N/A") sorted-solutions))
                   (into ["Overlap"] (mapv (partial formatted-objective :optimization-overlap "N/A") sorted-solutions))
                   (into ["Gene Count"] (mapv (comp str :gene-count) sorted-solutions))
                   (into ["Color"] (mapv
                                     (fn [{:keys [id]}]
                                       (or (get-in settings-map [:solutions, id, :color]) Color/BLACK))
                                     sorted-solutions)))]
    (when (or
            (not= old-columns new-columns)
            (not= old-rows new-rows))
      (props/set settings-table-data-prop
        {:columns new-columns,
         :rows new-rows}))))


(defn update-settings-map
  [settings-map, {:keys [rows, columns]}]
  (let [solution-settings-list (when (> (count rows) 1)
                                 (apply map vector rows))
        solution-id-list (into []
                           (comp
                             (remove #(-> % :id (= :attributes-column)))
                             (map :id))
                           columns)
        settings-from-table-map (zipmap solution-id-list
                                  (mapv
                                    ; ignore non-settings rows
                                    #(-> (zipmap [:caption, ::ignored, ::ignored, ::ignored, ::ignored, :color] %)
                                       (dissoc ::ignored))
                                    (rest solution-settings-list)))]
    (u/deep-merge settings-map {:solutions settings-from-table-map})))


(defn column-order
  [columns]
  (into []
    (comp
      (map :id)
      (filter number?))
    columns))


(defn row-order
  [rows]
  (into []
    (comp
      (filter #(= (:type %) :gene-row))
      (keep :gene-name))
    rows))


(defn sort-solutions
  [comparison-fn, comparison-table-data-prop, selected-solutions-prop]
  (let [sorted-solutions (->> (props/get selected-solutions-prop)
                           (sort comparison-fn)
                           (mapv :id))
        column-order-map (zipmap (-> (into [:genes-column] sorted-solutions) (conj :coverage-column)) (range))
        {:keys [rows, columns] :as table-data} (props/get comparison-table-data-prop)
        ; somehow `vec` does not work, so use `(into [] ...)`
        new-columns (into [] (sort-by (comp column-order-map :id) columns))]
    (props/set comparison-table-data-prop
      (assoc table-data
        ; keep row sort order (TableView sorting is resetted)
        :rows rows
        :columns new-columns))))


(defn gene-count-comparison
  [solution-1, solution-2]
  (compare (:gene-count solution-1), (:gene-count solution-2)))


(defn optimization-attribute-comparison
  [attribute, solution-1, solution-2]
  (let [coverage-1 (get solution-1 attribute)
        coverage-2 (get solution-2 attribute)]
    (cond
      (and coverage-1 coverage-2)
      (compare coverage-1, coverage-2)

      coverage-1 -1
      coverage-2 1
      :else (gene-count-comparison solution-1, solution-2))))


(defn group-rows
  [rows]
  (set/rename-keys (group-by :type rows) {:gene-row :gene-rows, :objective-row :objective-rows}))


(defn sort-alphabetical
  [ascending?, comparison-table-data-prop]
  (let [{:keys [rows] :as table-data} (props/get comparison-table-data-prop)
        {:keys [objective-rows, gene-rows]} (group-rows rows)]
    (props/set comparison-table-data-prop
      (assoc table-data
        :rows (into
                (vec
                  (sort-by :gene-name
                    (cond->> compare (not ascending?) (comp -))
                    gene-rows))
                objective-rows)))))


(defn compare-gene-names
  [row-1, row-2]
  ; alphabetical increasing
  (compare (:gene-name row-1) (:gene-name row-2)))


(defn sort-lexicographical
  [from-left?, comparison-table-data-prop]
  (let [{:keys [columns, rows] :as table-data} (props/get comparison-table-data-prop)
        {:keys [objective-rows, gene-rows]} (group-rows rows)
        solutions-column-keys (into [] (comp (map :attribute) (remove #(#{:gene-name, :coverage} %)))
                                (cond-> columns
                                  (not from-left?)
                                  reverse))
        tie-breaker (if (u/some-value #(= (:attribute %) :coverage) columns)
                      (fn [row-1, row-2]
                        ; decreasing
                        (let [result (compare (:coverage row-2) (:coverage row-1))]
                          (if (zero? result)
                            (compare-gene-names row-1, row-2)
                            result)))
                      compare-gene-names)
        sorted-rows (->> gene-rows
                      (mapv
                        (fn [index, {:keys [gene-name, coverage] :as row}]
                          {:index index
                           :gene-name gene-name
                           ; workaround to compare by number (quickfix)
                           :coverage (when coverage
                                       (if (= coverage "N/A")
                                         ; N/A values shall be smaller than actual coverage values
                                         Double/MIN_VALUE
                                         (try
                                           (Double/parseDouble (subs coverage 0, (dec (count coverage))))
                                           (catch NumberFormatException _
                                             ; Sorting by coverage as tie breaker is not important enough too fail with an exception
                                             ; use N/A handling
                                             Double/MIN_VALUE))))
                           :sort-data (mapv #(get row %) solutions-column-keys)})
                        (range))
                      (sort
                        (fn [{sd-1 :sort-data, :as row-1}, {sd-2 :sort-data, :as row-2}]
                          (let [sd-result (compare sd-2, sd-1)]
                            (if (zero? sd-result)
                              (tie-breaker row-1, row-2)
                              sd-result))))
                      (mapv
                        (fn [{:keys [index]}]
                          (nth gene-rows index))))]
    (props/set comparison-table-data-prop (assoc table-data :rows (into sorted-rows objective-rows)))))


(defn setup-sorting
  [children, comparison-table-data-prop, selected-solutions-prop]
  (let [{:keys [sort-solutions-ascending-gene-count-menu
                sort-solutions-descending-gene-count-menu
                sort-solutions-ascending-coverage-menu
                sort-solutions-descending-coverage-menu
                sort-solutions-ascending-overlap-menu
                sort-solutions-descending-overlap-menu
                sort-genes-lexicographical-left-right-menu
                sort-genes-lexicographical-right-left-menu
                sort-genes-alphabetical-ascending
                sort-genes-alphabetical-descending,
                comparison-table]} children
        setup-solution-sort (fn [menu, comparison-fn]
                              (jfx/set-menu-item-action! menu
                                (fn [_]
                                  (sort-solutions comparison-fn, comparison-table-data-prop, selected-solutions-prop)
                                  (table/clear-sort-order comparison-table))))
        setup-gene-sort (fn [menu, sort-fn]
                          (jfx/set-menu-item-action! menu
                            (fn [_]
                              (sort-fn comparison-table-data-prop)
                              (table/clear-sort-order comparison-table))))]
    ; solution sort
    (setup-solution-sort sort-solutions-ascending-gene-count-menu, gene-count-comparison)
    (setup-solution-sort sort-solutions-descending-gene-count-menu, (comp - gene-count-comparison))
    (setup-solution-sort sort-solutions-ascending-coverage-menu, (partial optimization-attribute-comparison :optimization-coverage))
    (setup-solution-sort sort-solutions-descending-coverage-menu, (comp - (partial optimization-attribute-comparison :optimization-coverage)))
    (setup-solution-sort sort-solutions-ascending-overlap-menu, (partial optimization-attribute-comparison :optimization-overlap))
    (setup-solution-sort sort-solutions-descending-overlap-menu, (comp - (partial optimization-attribute-comparison :optimization-overlap)))
    ; gene sort
    (setup-gene-sort sort-genes-lexicographical-left-right-menu (partial sort-lexicographical true))
    (setup-gene-sort sort-genes-lexicographical-right-left-menu (partial sort-lexicographical false))
    (setup-gene-sort sort-genes-alphabetical-ascending, (partial sort-alphabetical true))
    (setup-gene-sort sort-genes-alphabetical-descending, (partial sort-alphabetical false))))


(defn snapshot-alteration-type-set
  [snapshot]
  (get-in snapshot [:data, :alteration-type-set]))


(defn setup-coverage-column
  [dialog-state, coverage-column-combobox, alteration-type-combobox]
  (let [snapshots-prop (props/entry-property dialog-state, [:settings, :snapshots])
        selected-snapshot-prop (props/entry-property dialog-state [:settings, :selected-snapshot])
        alteration-types-prop (props/entry-property dialog-state, [:settings, :alteration-types])
        selected-alteration-type-prop (props/entry-property dialog-state, [:settings, :selected-alteration-type])
        snapshot-converter (jfx/string-converter
                             (fn [snapshot]
                               (if (= snapshot :coverage-snapshot/none)
                                 "NONE"
                                 (:name snapshot))))
        alteration-type-converter (jfx/string-converter
                                    (fn [x]
                                      (some-> x name (str/replace #"-" " "))))]

    ; select first alteration type
    (jfx/invalidation-listener! (props/items alteration-type-combobox)
      (fn [alteration-types]
        (when (seq alteration-types)
          (jfx/select-first! alteration-type-combobox))))

    ; determine alteration types
    (props/bind alteration-types-prop
      (props/fn-property
        (fn [snapshots, selected-solutions]
          (->> (->> snapshots
                 (mapcat snapshot-alteration-type-set)
                 distinct)
            (concat
              (->> selected-solutions
                (mapcat #(get-in % [:gene-sample-data, :alteration-type-set]))
                distinct))
            distinct
            sort
            vec))
        snapshots-prop
        (props/entry-property dialog-state, [:selected-solutions])))

    ; bind alteration types
    (props/list-property-binding (props/items alteration-type-combobox), alteration-types-prop
      :initialize :list)

    ; bind selected alteration type
    (props/bind selected-alteration-type-prop
      (-> (props/get-property alteration-type-combobox, :selection-model)
        (props/property :selected-item)))

    ; bind combobox items to snapshots
    (props/list-property-binding (props/items coverage-column-combobox),
      (props/fn-property
        (fn [snapshots, selected-alteration-type]
          (into [:coverage-snapshot/none]
            (filter
              (fn [snapshot]
                (contains? (snapshot-alteration-type-set snapshot) selected-alteration-type)))
            snapshots))
        snapshots-prop
        selected-alteration-type-prop)
      :initialize :list)

    ; bind selected snapshot
    (props/bind selected-snapshot-prop
      (-> (props/get-property coverage-column-combobox, :selection-model)
        (props/property :selected-item)))

    (doto coverage-column-combobox
      (jfx/combobox-converter! snapshot-converter)
      (jfx/combobox-cell-factory! snapshot-converter)
      jfx/select-first!)

    (doto alteration-type-combobox
      (jfx/combobox-converter! alteration-type-converter)
      (jfx/combobox-cell-factory! alteration-type-converter)))
  ; no return value
  nil)


(defn setup-objective-rows
  [dialog-state, children]
  (let [{:keys [measure-row-table
                add-measure-row-button
                remove-measure-row-button]} children
        _ (table/setup-table measure-row-table,
            [{:name "Measure"
              :attribute :objective
              :cell-factory (fn [_]
                              (table/combobox-cell
                                (props/create-property [:coverage, :overlap])
                                #(-> % name str/capitalize)
                                "Select an objective"))}
             {:name "Samples"
              :attribute :samples
              :cell-factory (fn [_]
                              (table/combobox-cell
                                (props/create-property [:optimization-group, :all])
                                #(case %
                                   :optimization-group "Optimization Group"
                                   :all "All")
                                "Select a sample group"))}]
            {:placeholder "No measure rows added."})
        rows-prop (table/rows-property measure-row-table)]

    (props/bind (props/entry-property dialog-state, [:settings, :objective-rows])
      (props/fn-property
        (fn [rows]
          (filterv (fn [{:keys [objective, samples]}] (and objective samples)) rows))
        rows-prop))

    (props/bind (props/property add-measure-row-button, :disable)
      (props/fn-property nil?
        (props/entry-property dialog-state, [:settings, :selected-alteration-type])))

    (jfx/handle-event! add-measure-row-button, :action
      (fn [_]
        (props/swap rows-prop conj {:objective nil, :samples nil})))


    (props/bind (props/property remove-measure-row-button, :disable)
      (props/fn-property empty?, rows-prop))

    (jfx/handle-event! remove-measure-row-button, :action
      (fn [_]
        (props/swap rows-prop pop)))))


(defn setup-font-button
  [dialog-state, path, button]
  (let [font-prop (props/entry-property dialog-state, path)]
    (props/bind (props/property button, :font)
      (props/fn-property
        (fn [font-map]
          ; override size for button font to prevent growing button height dependent on chosen size
          (jfx/font (assoc font-map :size 13)))
        font-prop))
    (jfx/handle-event! button, :action
      (fn font-button-clicked [_]
        (when-let [selected-font (font-sel/select-font {:initial-font (props/get font-prop)})]
          (props/set font-prop (jfx/font-map selected-font)))))))


(defn setup-length-textfield
  [dialog-state, path, default-value, textfield]
  (props/bind-bidi
    (props/property textfield, :text)
    #(try
       (Long/parseLong %)
       (catch Throwable t
         default-value))
    (props/entry-property dialog-state, path)
    str))


(defn setup-style-settings
  [dialog-state
   {:keys [coverage-label-textfield
           coverage-label-font-button
           coverage-value-font-button
           coverage-column-width-textfield
           column-width-textfield
           solution-caption-font-button
           gene-column-width-textfield
           gene-column-label-textfield
           gene-column-label-font-button
           gene-font-button
           row-height-textfield
           header-height-textfield
           objective-label-font-button
           objective-value-font-button]
    :as children}]
  ; coverage label
  (props/bind-bidi
    (props/property coverage-label-textfield, :text)
    (props/entry-property dialog-state, [:settings, :coverage-label]))
  ; coverage label font
  (setup-font-button dialog-state, [:settings, :coverage-label-font], coverage-label-font-button)
  ; coverage value font
  (setup-font-button dialog-state, [:settings, :coverage-value-font], coverage-value-font-button)
  ; coverage column width
  (setup-length-textfield dialog-state, [:settings, :coverage-column-width], 50, coverage-column-width-textfield)
  ; solution column width
  (setup-length-textfield dialog-state, [:settings, :solution-column-width], 50, column-width-textfield)
  ; solution caption font
  (setup-font-button dialog-state, [:settings, :solution-caption-font], solution-caption-font-button)
  ; gene column label
  (props/bind-bidi
    (props/property gene-column-label-textfield, :text)
    (props/entry-property dialog-state, [:settings, :gene-column-label]))
  ; gene column label font
  (setup-font-button dialog-state, [:settings, :gene-column-label-font], gene-column-label-font-button)
  ; gene column width
  (setup-length-textfield dialog-state, [:settings, :gene-column-width], 100, gene-column-width-textfield)
  ; gene font
  (setup-font-button dialog-state, [:settings, :gene-font], gene-font-button)
  ; row height
  (setup-length-textfield dialog-state, [:settings, :row-height], 30, row-height-textfield)
  ; header height
  (setup-length-textfield dialog-state, [:settings, :header-height], 30, header-height-textfield)
  ; objective label font
  (setup-font-button dialog-state, [:settings, :objective-label-font], objective-label-font-button)
  ; objective value font
  (setup-font-button dialog-state, [:settings, :objective-value-font], objective-value-font-button))


(defn export-graphic
  [window, table-config]
  #_(inspect table-config)
  #_(vsvg/view (sce/render-table table-config))
  (when-let [file (jfx/choose-file window, "Export Solution Comparison", :save
                    {:file-types (u/extension-map "pdf" "svg" "png", "tiff")})]
    (u/safe-future
      (progress/with-shown-indeterminate-progress (format "Exporting Solution Comparsion to \"%s\" ..." (.getName file))
        (sce/export-table table-config, file)))))

; TODO: maybe implement later
#_(defn show-solution-in-statistics-view
    [data-mgmt, selected-solutions-prop, solution-id, event]
    (when (jfx/double-click? event)
      (when-let [solution (u/some-value #(= (:id %) solution-id) (props/get selected-solutions-prop))]
        (sv/show-objectives-view data-mgmt, (:gene-sample-data solution), (dm/plot-style data-mgmt)))))


(defn show-dialog
  [ui]
  (jfx/run-now
    (let [default-font {:family "Arial", :weight :normal, :posture :regular, :size 13}
          default-bold-font (assoc default-font :weight :bold)
          default-italic-font (assoc default-font :posture :italic)
          dialog-state-atom (atom
                              {:tree (solution-selection-tree-data ui)
                               :settings {:rotate-column-captions? true,
                                          :solution-column-width 50
                                          :row-height 25
                                          :header-height 100
                                          :snapshots (dm/snapshots (:data-management ui))
                                          :solutions {}
                                          :gene-column-label "Gene"
                                          :gene-column-label-font default-bold-font
                                          :gene-column-width 100
                                          :gene-font default-italic-font
                                          :coverage-column-width 50
                                          :coverage-label "Coverage"
                                          :coverage-label-font default-bold-font
                                          :coverage-value-font default-font
                                          :solution-caption-font default-bold-font
                                          :objective-label-font default-font
                                          :objective-value-font default-font}})
          dialog-state (props/data-property dialog-state-atom)
          control (jfx/create-control "avatar/ui/GeneComparisonView.fxml")
          control-node (jfx/control-node control)
          {:keys [selection-treetable
                  comparison-table
                  settings-table
                  rotate-columns-checkbox
                  export-graphic-button
                  sort-genes-menu-button
                  sort-solutions-menu-button
                  alteration-type-combobox
                  coverage-column-combobox]
           :as children} (jfx/control-children control)
          window (doto (jfx/window "Solution Comparison", control-node)
                   (jfx/user-data! dialog-state-atom)
                   (t/add-avatar-icon))
          selected-solutions-prop (props/entry-property dialog-state, [:selected-solutions])
          settings-map-prop (props/entry-property dialog-state, [:settings])
          _ (table/setup-table comparison-table, [],
              {:sort? false,
               :placeholder "No gene sets selected."})
          _ (table/setup-table settings-table, [{:name "Attributes"}]
              {:sort? false,
               :placeholder "Nothing to display."})
          comparison-table-data-prop (table/data-property comparison-table)
          settings-table-data-prop (table/data-property settings-table)
          no-data-selected?-prop (props/fn-property
                                   (fn [{:keys [columns, rows]}]
                                     (or (empty? columns) (empty? rows)))
                                   comparison-table-data-prop)]

      #_(u/watch-state-changes dialog-state-atom)

      (props/swap comparison-table-data-prop assoc
        :columns
        [{:name "Genes"
          :id :genes-column
          :attribute :gene-name
          :width 100
          :sortable? false
          :reorderable? false
          :cell-factory (partial gene-column-cell-factoy settings-map-prop (table/rows-property comparison-table))}])


      (props/bind (props/property comparison-table, :fixed-cell-size)
        (props/entry-property dialog-state, [:settings, :row-height]))

      (table/bind-table-header-height! comparison-table
        (props/entry-property dialog-state, [:settings, :header-height]))

      (jfx/bind-bidirectional
        (props/property rotate-columns-checkbox, :selected)
        (props/entry-property dialog-state, [:settings, :rotate-column-captions?]))

      (setup-coverage-column dialog-state, coverage-column-combobox, alteration-type-combobox)
      (setup-objective-rows dialog-state, children)

      (jfx/bind selected-solutions-prop
        (props/fn-property
          (fn [selected-nodes]
            (filterv (fn [{:keys [type]}] (#{:solution-item, :snapshot-item} type)) selected-nodes))
          (treetable/selected-leaves-property dialog-state)))

      ; determine comparison table content based on the selected solutions
      (props/bind-updating comparison-table-data-prop,
        (partial update-gene-table-data
          (partial gene-membership-cell-factory settings-map-prop)
          (fn [_]
            (table/text-cell-custom-font :center-right
              (props/entry-property dialog-state, [:settings, :coverage-value-font])))
          #_(partial show-solution-in-statistics-view (:data-management ui), selected-solutions-prop))
        settings-map-prop
        selected-solutions-prop)

      (jfx-select/enable-multiple-selection! comparison-table)
      (jfx/enable-copy-to-clipboard! comparison-table)

      ; settings table
      (jfx/listen-to (partial update-settings-table-data settings-table-data-prop)
        selected-solutions-prop
        (table/columns-property comparison-table)
        settings-map-prop)

      (props/bind-updating settings-map-prop,
        (fn [settings-map, _, [new-settings-table-data]]
          (update-settings-map settings-map, new-settings-table-data))
        settings-table-data-prop)


      (setup-style-settings dialog-state, children)


      ; hide table header of settings table
      (table/hide-table-header settings-table)

      #_(jfx/change-listener! settings-map-prop
          (fn [_, old, new]
            (u/print-modifications "update settings map" old, new)))

      #_(jfx/change-listener! comparison-table-data-prop
          (fn [_, old, new]
            (u/print-modifications "comparison table" old, new)))

      (jfx/bind (props/property sort-genes-menu-button, :disable) no-data-selected?-prop)
      (jfx/bind (props/property sort-solutions-menu-button, :disable) no-data-selected?-prop)
      (setup-sorting children, comparison-table-data-prop, selected-solutions-prop)

      (jfx/bind (props/property export-graphic-button :disable) no-data-selected?-prop)

      (jfx/handle-event! export-graphic-button, :action
        (fn [_]
          (let [{:keys [rows, columns]} (props/get comparison-table-data-prop)]
            (export-graphic window,
              {:column-order (column-order columns)
               :row-order (row-order rows)
               :selected-solutions (props/get selected-solutions-prop)
               :settings (props/get settings-map-prop)}))))

      (treetable/setup-treetable selection-treetable, dialog-state
        [{:name "Name",
          :attribute :name
          :sortable? false}
         {:attribute :checked?,
          :cell-factory (fn [_] (treetable/checkbox-cell))}
         {:attribute :gene-count
          :sortable? true
          :cell-factory (fn [_] (treetable/number-cell "%d"))}
         {:attribute :sample-count
          :sortable? true
          :cell-factory (fn [_] (treetable/number-cell "%d"))}
         {:attribute :optimization-coverage
          :sortable? true
          :cell-factory (fn [_] (treetable/number-cell "%.3f"))}
         {:attribute :optimization-overlap
          :sortable? true
          :cell-factory (fn [_] (treetable/number-cell "%.3f"))}])

      (jfx/show window))))
