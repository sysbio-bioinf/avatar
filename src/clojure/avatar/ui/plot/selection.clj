; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.selection
  (:require
    [avatar.ui.plot.data :as pdata]
    [avatar.data.operations :as ops]
    [avatar.util :as u]
    [clojure.set :as set]
    [clj-jfx.core :as jfx]
    [clj-jfx.colors :as col]
    [clojure.string :as str]
    [avatar.ui.tools :as t])
  (:import (javafx.scene.control ToggleButton TextArea)
           (javafx.scene.shape Rectangle)))



(defn opposite-header-type
  [header-type]
  (case header-type
    :row :column,
    :column :row))


(defn selected-headers
  [selection-state-map, header-type]
  (get-in selection-state-map [:selected-header-map, header-type]))


(defn deselect-opposite-headers
  [selection-state-map, header-type]
  (assoc-in selection-state-map [:selected-header-map (opposite-header-type header-type)] #{}))


(defn only-fixed-or-unfixed-rows?
  ([selection-state-map, plot-data]
   (only-fixed-or-unfixed-rows? selection-state-map, plot-data, nil))
  ([selection-state-map, plot-data, header-index]
   (let [fixed-row? (partial pdata/fixed-row? plot-data),
         fixed?-states (-> (selected-headers selection-state-map, :row)
                         (cond-> header-index (conj header-index))
                         (->>
                           (map fixed-row?)
                           set))]
     (= (count fixed?-states) 1))))


(defn update-header-selection
  [selection-state-map, plot-data, header-type, header-data-index, selected?]
  (let [selection-state-map (cond-> selection-state-map
                              ; when header selected, then deselect all headers of the opposite header type
                              selected? (deselect-opposite-headers header-type))]
    (if-not selected?
      ; remove header from selection
      (update-in selection-state-map [:selected-header-map, header-type] disj header-data-index)
      ; add header to selection
      (case header-type
        ; just add column header
        :column
        (update-in selection-state-map [:selected-header-map :column] conj header-data-index)
        ; ensure that only fixed or unfixed row headers are selected at the same time
        :row
        (if (only-fixed-or-unfixed-rows? selection-state-map, plot-data, header-data-index)
          (update-in selection-state-map [:selected-header-map :row] conj header-data-index)
          (assoc-in selection-state-map [:selected-header-map :row] #{header-data-index}))))))



(defn on-header-selected
  [plot, header-type, header-view-index, selected?]
  (dosync
    (let [{:keys [selection, state]} (deref plot),
          {:keys [plot-data, view]} state,
          header-data-index (pdata/header-view-index->data-index view, header-type, header-view-index)]
      (alter plot assoc :selection
        (update-header-selection selection, plot-data, header-type, header-data-index, selected?)))))


(defn select-headers
  [{:keys [selected-header-map] :as selection-state-map}, fixed-row?, header-type, header-fixed?, header-data-indices]
  ; deselect all headers of the opposite header type
  (let [selection-state-map (deselect-opposite-headers selection-state-map, header-type)]
    ; add headers to selection
    (case header-type
      ; just add column headers
      :column
      (update-in selection-state-map [:selected-header-map, :column] into header-data-indices)
      ; ensure that only fixed or unfixed row headers are selected at the same time
      :row
      (update-in selection-state-map [:selected-header-map, :row]
        (fn [selected-row-header-indices]
          (->> header-data-indices
            (concat selected-row-header-indices)
            (filter
              (fn [header-index]
                (= (fixed-row? header-index) header-fixed?)))
            set))))))


(defn select-headers-from-to
  [plot, header-type, data-from-index, data-to-index]
  (dosync
    (let [{:keys [selection, state]} (deref plot)
          {:keys [plot-data, view]} state
          fixed-row? (fn [index] (and (= header-type :row) (pdata/fixed-row? plot-data, index)))
          header-fixed? (fixed-row? data-from-index)
          ; only select rows that have the same fixed-state as the clicked header
          affected-headers (cond->> (if (< data-from-index data-to-index)
                                      (range data-from-index (inc data-to-index))
                                      (range data-to-index (inc data-from-index)))
                             header-fixed? (filter fixed-row?)
                             (not header-fixed?) (remove fixed-row?))]
      (alter plot assoc :selection (select-headers selection, fixed-row?, header-type, header-fixed?, affected-headers)))))



(defn deselect-headers
  [selection-state-map, header-type, header-data-indices]
  ; remove all headers from selection map
  (update-in selection-state-map [:selected-header-map, header-type] #(reduce disj % header-data-indices)))


(defn on-select-group
  [plot, header-type, event]
  (dosync
    (let [{:keys [selection, state]} (deref plot)
          {:keys [header-view-index]} (pdata/contextmenu-data plot)
          {:keys [plot-data, view]} state
          fixed-row? (fn [index] (and (= header-type :row) (pdata/fixed-row? plot-data, index)))
          header-data-index (pdata/header-view-index->data-index view, header-type, header-view-index)
          group-members (case header-type
                          :row (ops/gene-group-member-indices plot-data, [header-data-index])
                          :column (ops/sample-group-member-indices plot-data, [header-data-index]))
          header-fixed? (fixed-row? header-data-index)
          ; only modify group members that have the same fixed-state as the clicked header
          affected-group-members (cond->> group-members
                                   header-fixed? (filter fixed-row?)
                                   (not header-fixed?) (remove fixed-row?))]
      (alter plot assoc :selection (select-headers selection, fixed-row?, header-type, header-fixed?, affected-group-members)))))


(defn on-select-all
  [plot, header-type, event]
  (dosync
    (let [{:keys [selection, state]} (deref plot)
          {:keys [plot-data, view]} state
          fixed-row? (fn [index] (and (= header-type :row) (pdata/fixed-row? plot-data, index)))
          {:keys [header-view-index]} (pdata/contextmenu-data plot)
          header-data-index (pdata/header-view-index->data-index view, header-type, header-view-index)
          new-selection (case header-type
                          :row (range (->> plot-data :gene-list count))
                          :column (range (:sample-count plot-data)))
          header-fixed? (fixed-row? header-data-index)
          new-selection (cond->> new-selection
                          header-fixed? (filter fixed-row?)
                          (not header-fixed?) (remove fixed-row?))]
      (alter plot assoc :selection (select-headers selection, fixed-row?, header-type, header-fixed?, new-selection)))))


(defn select-genes
  [plot, gene-list]
  (dosync
    (let [{:keys [selection, state]} (deref plot)
          {:keys [plot-data, view]} state
          gene-set (set gene-list),
          data-indices (persistent!
                         (u/reduce-indexed
                           (fn [data-indices, index, gene]
                             (cond-> data-indices
                               (contains? gene-set gene) (conj! index)))
                           (transient [])
                           (:gene-list plot-data))),
          fixed-row? (fn [index] (pdata/fixed-row? plot-data, index)),
          [header-fixed?, data-indices] (let [{fixed-gene-indices true, not-fixed-gene-indices false} (group-by fixed-row? data-indices)
                                              ; majority vote whether to select the fixed or not fixed genes
                                              fixed? (> (count fixed-gene-indices) (count not-fixed-gene-indices))]
                                          [fixed?, (if fixed? fixed-gene-indices not-fixed-gene-indices)])]
      (alter plot assoc :selection (select-headers selection, fixed-row?, :row, header-fixed?, data-indices)))))



(defn on-deselect-group
  [plot, header-type, event]
  (dosync
    (let [{:keys [selection, state]} (deref plot)
          {:keys [header-view-index]} (pdata/contextmenu-data plot)
          {:keys [plot-data, view]} state
          header-data-index (pdata/header-view-index->data-index view, header-type, header-view-index)
          group-members (case header-type
                          :row (ops/gene-group-member-indices plot-data, [header-data-index])
                          :column (ops/sample-group-member-indices plot-data, [header-data-index]))]
      (alter plot assoc :selection (deselect-headers selection, header-type, group-members)))))


(defn inverted-row-set
  [plot-state, selected-set]
  (let [{:keys [view, plot-data]} plot-state,
        {:keys [gene-count]} view,
        fixed-row? #(pdata/fixed-row? plot-data, %),
        prev-selected-fixed? (boolean (some fixed-row? selected-set))
        ; invert within fixed/not-fixed partition
        include? (if prev-selected-fixed?
                   (fn [row-index]
                     (and
                       ; not in previous selection
                       (not (contains? selected-set row-index))
                       ; fixed
                       (fixed-row? row-index)))
                   (fn [row-index]
                     (and
                       ; not in previous selection
                       (not (contains? selected-set row-index))
                       ; not fixed
                       (not (fixed-row? row-index)))))]
    (persistent!
      (reduce
        (fn [inverted-set, row-index]
          (cond-> inverted-set
            (include? row-index)
            (conj! row-index)))
        (transient #{})
        (set (range gene-count))))))


(defn on-invert-selection
  [plot, header-type, event]
  (dosync
    (let [{:keys [selection, state]} (deref plot),
          {:keys [gene-count, sample-count]} (:view state),
          selected-set (get-in selection [:selected-header-map, header-type]),
          inverted-set (case header-type
                         :column (set/difference (set (range sample-count)) selected-set)
                         :row (inverted-row-set state, selected-set))]
      (alter plot assoc-in [:selection, :selected-header-map, header-type] inverted-set))))


(defn selection-overlay-map
  [controls-map, overlay-type]
  (get-in controls-map [:selection-overlay-map, overlay-type]))


(defn selection-overlay
  [controls-map, overlay-type, index]
  (get-in controls-map [:selection-overlay-map, overlay-type, index]))


(defn add-selection-overlay
  [controls-map, overlay-type, index, overlay-node]
  (assoc-in controls-map [:selection-overlay-map, overlay-type, index] overlay-node))


(defn remove-selection-overlay
  [controls-map, overlay-type, index]
  (update-in controls-map [:selection-overlay-map, overlay-type] dissoc index))


(defn row-bounding-rect
  [{:keys [gene-group-width,
           gene-name-width,
           alteration-canvas-width,
           alteration-ratio-width,
           alteration-row-height,
           row-header-width,
           column-header-height,
           sample-group-height] :as plot-layout},
   row-index]
  (vector
    (+ row-header-width gene-group-width),
    (+ column-header-height sample-group-height (* row-index alteration-row-height)),
    (+ gene-name-width alteration-canvas-width alteration-ratio-width),
    alteration-row-height))


(defn column-bounding-rect
  [{:keys [alteration-canvas-height,
           alteration-cell-width,
           sample-group-height,
           gene-group-width,
           gene-name-width
           row-header-width,
           column-header-height] :as plot-layout},
   column-index]
  (vector
    (+ row-header-width gene-group-width gene-name-width (* column-index alteration-cell-width)),
    (+ column-header-height sample-group-height),
    alteration-cell-width,
    alteration-canvas-height))


(defn selection-rectangle
  [x, y, width, height, color]
  (doto (Rectangle. x, y, width, height)
    (jfx/shape-fill! (col/color color, 0.33))))


(defn show-selection-overlay!
  [{:keys [overlay-pane] :as controls-map}, {:keys [plot-style, plot-layout] :as plot-state-map}, header-type, index]
  (if-not (selection-overlay controls-map, header-type, index)
    (let [{:keys [selection-color]} plot-style
          [x, y, width, height] (case header-type
                                  :row (row-bounding-rect plot-layout, index)
                                  :column (column-bounding-rect plot-layout, index))
          selection-rect (selection-rectangle x, y, width, height, selection-color)]
      (jfx/add-child overlay-pane, selection-rect)
      (add-selection-overlay controls-map, header-type, index, selection-rect))
    controls-map))


(defn hide-selection-overlay!
  [{:keys [overlay-pane] :as controls-map}, header-type, index]
  (if-let [selection-rect (selection-overlay controls-map, header-type, index)]
    (do
      (jfx/remove-children overlay-pane, selection-rect)
      (remove-selection-overlay controls-map, header-type, index))
    controls-map))


(defn header-view-index
  [{:keys [visible-gene-count, visible-sample-count] :as view}, header-type, header-data-index]
  (let [header-view-index (pdata/header-data-index->view-index view, header-type, header-data-index)
        upper-limit (case header-type
                      :row visible-gene-count
                      :column visible-sample-count)]
    (when (and (<= 0 header-view-index) (< header-view-index upper-limit))
      header-view-index)))


(defn update-header-selected-states
  [controls-map, view, selected-header-map, header-type]
  (let [header-controls (pdata/headers controls-map, header-type),
        selected-headers (get selected-header-map header-type)]
    (u/for-each-indexed!
      (fn [header-view-index, ^ToggleButton header]
        (let [header-data-index (pdata/header-view-index->data-index view, header-type, header-view-index),
              selected? (contains? selected-headers header-data-index)]
          (.setSelected header selected?)))
      header-controls)))


(defn update-selection-overlays
  [controls-map, {:keys [view] :as plot-state-map}, selected-header-map, header-type]
  (let [selected-index-set (get selected-header-map header-type),
        visibile-view-index-set (set (keep #(header-view-index view, header-type, %) selected-index-set))
        ; hide old overlays of previous selections
        controls-map (reduce
                       (fn [controls-map, header-index]
                         (hide-selection-overlay! controls-map, header-type, header-index))
                       controls-map
                       ; header selection overlays that need to be removed
                       (set/difference (-> controls-map (selection-overlay-map header-type) keys set) visibile-view-index-set)),
        ; show newly selected overlays
        controls-map (reduce
                       (fn [controls-map, header-index]
                         (show-selection-overlay! controls-map, plot-state-map, header-type, header-index))
                       controls-map
                       visibile-view-index-set)]
    controls-map))



(defn row-group-bounding-rect
  [{:keys [gene-group-width,
           alteration-row-height,
           row-header-width,
           column-header-height,
           sample-group-height] :as plot-layout},
   group-size,
   row-index]
  (vector
    row-header-width,
    (+ column-header-height sample-group-height (* row-index alteration-row-height))
    gene-group-width,
    (* group-size alteration-row-height)))


(defn column-group-bounding-rect
  [{:keys [alteration-cell-width,
           sample-group-height,
           gene-group-width,
           gene-name-width
           row-header-width,
           column-header-height] :as plot-layout},
   group-size,
   column-index]
  (vector
    (+ row-header-width gene-group-width gene-name-width (* column-index alteration-cell-width)),
    column-header-height,
    (* group-size alteration-cell-width),
    sample-group-height))


(defn show-group-selection-overlay!
  [{:keys [overlay-pane] :as controls-map}, {:keys [plot-style, plot-layout] :as plot-state-map}, header-group-type, header-type, group-size, index]
  (let [{:keys [selection-color]} plot-style
        [x, y, width, height] (case header-type
                                :row (row-group-bounding-rect plot-layout, group-size, index)
                                :column (column-group-bounding-rect plot-layout, group-size, index))
        selection-rect (selection-rectangle x, y, width, height, selection-color)]
    (jfx/add-child overlay-pane, selection-rect)
    (add-selection-overlay controls-map, header-group-type, index, selection-rect)))


(defn selected-visible-groups
  [{:keys [view, group-map] :as plot-state-map}, header-type, selected-header-data-index-set]
  (reduce
    (fn [result-groups, {:keys [indices]}]
      (let [group-selected? (some #(contains? selected-header-data-index-set %) indices)
            group-view-index-set (set (keep #(header-view-index view, header-type, %) indices))]
        (cond-> result-groups
          ; group must be selected by at least one header and at least one element of the group must be in the view
          (and group-selected? (seq group-view-index-set))
          (conj {:start-index (reduce min group-view-index-set),
                 :group-size (count group-view-index-set)}))))
    []
    (get group-map header-type)))


(defn update-group-selection-overlays
  [controls-map, plot-state-map, selected-header-map, header-type]
  (let [header-group-type (keyword (format "%s-group" (name header-type))),
        selected-header-data-index-set (get selected-header-map header-type),
        visible-groups (selected-visible-groups plot-state-map, header-type, selected-header-data-index-set)
        ; hide old overlays of previous selections
        controls-map (reduce
                       (fn [controls-map, header-index]
                         (hide-selection-overlay! controls-map, header-group-type, header-index))
                       controls-map
                       ; all header selection overlays need to be removed
                       (-> controls-map (selection-overlay-map header-group-type) keys set)),
        ; show newly selected overlays
        controls-map (reduce
                       (fn [controls-map, {:keys [start-index, group-size]}]
                         (show-group-selection-overlay! controls-map, plot-state-map,
                           header-group-type, header-type, group-size, start-index))
                       controls-map
                       visible-groups)]
    controls-map))


(defn update-all-selection-overlays
  [controls-map, plot-state-map, selected-header-map]
  (-> controls-map
    (update-selection-overlays plot-state-map, selected-header-map, :row)
    (update-selection-overlays plot-state-map, selected-header-map, :column)
    (update-group-selection-overlays plot-state-map, selected-header-map, :row)
    (update-group-selection-overlays plot-state-map, selected-header-map, :column)))


(defn setup-selection-rendering
  [plot, selection-change-handler]
  (let [selected-header-map-property (jfx/map-entry-property plot, [:selection, :selected-header-map])]
    ; header selected states
    (jfx/listen-to
      (fn [view, selected-header-map]
        (when view
          (let [controls-map (:controls (deref plot))]
            (update-header-selected-states controls-map, view, selected-header-map, :row)
            (update-header-selected-states controls-map, view, selected-header-map, :column))))
      (jfx/map-entry-property plot, [:state, :view])
      selected-header-map-property)
    ; notify on selection changes
    (when selection-change-handler
      (jfx/listen-to
        (fn [selected-header-map]
          (selection-change-handler selected-header-map))
        selected-header-map-property)))
  ; selection overlays
  (jfx/listen-to
    (fn [plot-state-map, selected-header-map]
      (let [controls-map (:controls (deref plot)),
            controls-map (update-all-selection-overlays controls-map, plot-state-map, selected-header-map)]
        (dosync (alter plot assoc :controls controls-map))))
    (jfx/map-entry-property plot, [:state])
    (jfx/map-entry-property plot, [:selection, :selected-header-map])))


(defn rerender-selection-overlays
  [plot]
  (dosync
    (let [{:keys [state, controls, selection]} (ensure plot)
          {:keys [selected-header-map]} selection
          controls (-> controls
                     ; delete overlays
                     (update-all-selection-overlays state, {})
                     ; readd overlays
                     (update-all-selection-overlays state, selected-header-map))]
      (alter plot assoc :controls controls))))


(defn clear-header-selection!
  [plot]
  (dosync
    (alter plot update-in [:selection] assoc :selected-header-map {:column #{}, :row #{}})
    plot))



(defn on-show-sample-ids
  [plot]
  (let [selected-sample-columns (sort (pdata/selected-headers plot, :column))
        {:keys [sample-id-vec, sample-permutation]} (pdata/data plot)]
    (if (seq sample-id-vec)
      (when (seq selected-sample-columns)
        (let [{:keys [column->sample]} sample-permutation,
              selected-ids (mapv #(nth sample-id-vec (get column->sample %)) selected-sample-columns)
              control (jfx/create-control "avatar/ui/SelectedSamplesView.fxml"),
              control-node (jfx/control-node control),
              {:keys [^TextArea sample-id-textarea, close-button]} (jfx/control-children control),
              window (doto (jfx/modal-window "Selected sample ids", control-node, 600, 300)
                       (t/add-avatar-icon))]
          (jfx/property-value! sample-id-textarea, :text, (str/join "\n" selected-ids))
          (jfx/handle-event! close-button :action (fn [_] (jfx/close window)))
          (jfx/show-and-wait window)))
      (jfx/show-information "Show selected sample ids", "There are no sample ids in this dataset.", "You need to provide sample ids in the files that you import."))))


(defn on-show-gene-names
  [plot]
  (let [selected-gene-indices (sort (pdata/selected-headers plot, :row))
        {:keys [gene-list]} (pdata/data plot)]
    (when (seq selected-gene-indices)
      (let [selected-gene-names (mapv #(:gene (nth gene-list %)) selected-gene-indices)
            control (jfx/create-control "avatar/ui/SelectedSamplesView.fxml"),
            control-node (jfx/control-node control),
            {:keys [^TextArea sample-id-textarea, close-button]} (jfx/control-children control),
            window (doto (jfx/modal-window "Selected gene names", control-node, 600, 300)
                     (t/add-avatar-icon))]
        (jfx/property-value! sample-id-textarea, :text, (str/join "\n" selected-gene-names))
        (jfx/handle-event! close-button :action (fn [_] (jfx/close window)))
        (jfx/show-and-wait window)))))


(defn selected-header-type
  [plot]
  (let [{:keys [row, column]} (get-in (deref plot) [:selection, :selected-header-map])]
    (cond
      (seq row) :row
      (seq column) :column)))