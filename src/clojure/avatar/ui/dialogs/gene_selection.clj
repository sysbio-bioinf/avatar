; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.gene-selection
  (:require [clj-jfx.core :as jfx]
            [avatar.algorithms.column-sort :as col-sort]
            [avatar.ui.tools :as t]
            [clj-jfx.selection :as jfx-select]
            [avatar.ui.plot :as plot]
            [avatar.ui.plot-export :as pexp]
            [avatar.ui.dialogs.statistics-view :as sv]
            [avatar.ui.dialogs.gene-search :as gsearch]
            [avatar.util :as u]
            [clojure.string :as str])
  (:import (javafx.scene.layout StackPane)
           (javafx.scene.control ListView MultipleSelectionModel ComboBox Button TextArea Label)
           (javafx.geometry Bounds)))



(defn str->gene-name-list
  [s]
  (into []
    (comp
      (map str/trim)
      (remove str/blank?)
      (distinct))
    (str/split s, #"[,\s]")))


(defn query-gene-list
  [complete-fn]
  (let [control (jfx/create-control "avatar/ui/GeneListInputDialog.fxml"),
        control-node (jfx/control-node control),
        {:keys [^Button find-button,
                ^TextArea
                gene-list-textarea,
                ^Label
                search-term-number-label,
                cancel-button]} (jfx/control-children control)
        dialog-data (atom {}),
        search-gene-list-prop (jfx/map-entry-property dialog-data, [:search-gene-list]),
        window (doto (jfx/modal-window "Enter list of genes", control-node, 1000, 500)
                 (t/add-avatar-icon))]


    (jfx/bind search-gene-list-prop
      (jfx/functional-property str->gene-name-list
        (jfx/property gene-list-textarea, :text)))

    (jfx/bind (jfx/property find-button, :disable)
      (jfx/functional-property
        empty?
        search-gene-list-prop))


    (jfx/bind (jfx/property search-term-number-label, :text)
      (jfx/functional-property
        (comp str count)
        search-gene-list-prop))

    (jfx/handle-event! find-button, :action,
      (fn [_]
        (complete-fn (jfx/value search-gene-list-prop))
        (jfx/close window)))

    (jfx/handle-event! cancel-button, :action,
      (fn [_]
        (jfx/close window)))

    (jfx/show window)))



(defn select-genes-by-name
  [available-gene-list, gene-names-list]
  (filterv
    (comp (set gene-names-list) :gene)
    available-gene-list))


(defn update-group-data
  [dialog-data, available-gene-list, update-fn, selected-gene-names-set]
  (when (seq selected-gene-names-set)
    (let [selected-genes (select-genes-by-name available-gene-list, selected-gene-names-set)]
      (swap! dialog-data update-in [:group-data] update-fn selected-genes))))


(defn update-group-data-based-on-selection
  [dialog-data, available-gene-list, ^ListView listview, update-fn]
  (let [^MultipleSelectionModel selection-model (jfx/property-value listview :selection-model)
        selected-gene-names (set (.getSelectedItems selection-model))]
    (update-group-data dialog-data, available-gene-list, update-fn, selected-gene-names)))

(defn add-genes
  [alteration-type, group-data, selected-genes]
  (col-sort/lexicographic-sort true, false, alteration-type,
    (update-in group-data [:gene-list] #(->> selected-genes (into %) distinct vec))))

(defn remove-genes
  [alteration-type, group-data, selected-genes]
  (col-sort/lexicographic-sort true, false, alteration-type,
    (update-in group-data [:gene-list] #(vec (remove (set selected-genes) %)))))


(defn sort-samples
  [dialog-data, alteration-type]
  (swap! dialog-data update-in [:group-data] (partial col-sort/lexicographic-sort true, false, alteration-type)))


(defn find+add-gene
  [ui, dialog-state, alteration-type, available-gene-list]
  (gsearch/show-dialog ui, (mapv :gene available-gene-list), :single-gene, nil,
    (fn [selected-gene-name]
      (update-group-data dialog-state, available-gene-list, (partial add-genes alteration-type), [selected-gene-name]))))


(defn find+add-genes-from-list
  [ui, dialog-state, alteration-type, available-gene-list]
  (query-gene-list
    (fn [search-gene-name-list]
      (let [add-genes (fn [gene-name-list]
                        (update-group-data dialog-state, available-gene-list, (partial add-genes alteration-type), gene-name-list))
            exact-matches (select-genes-by-name available-gene-list, search-gene-name-list)
            matching-gene-name-list (mapv :gene exact-matches),
            search-gene-name-list (u/removev (set matching-gene-name-list) search-gene-name-list)]
        ; add exact matches
        (add-genes matching-gene-name-list)
        ; ask for user lookup
        (when (and
                (seq search-gene-name-list)
                (jfx/ask-confirmation "Find & add gene list",
                  "Do you want to choose matches for the unmatched symbols?"
                  (format "Not all symbols could be matched to genes in the data.\nFound %d genes with matching names (no matches: %d)"
                    (count matching-gene-name-list)
                    (count search-gene-name-list))))
          ; search genes for unmatched symbols and add the result
          (gsearch/show-dialog ui, (mapv :gene available-gene-list), :multiple-genes, search-gene-name-list, add-genes))))))


(defn show-dialog
  [ui, {:keys [gene-list, alteration-type-set] :as gene-sample-data},
   {:keys [shown-alteration-type] :as plot-style}, complete-fn]
  (jfx/run-now
    (let [empty-group-data (assoc gene-sample-data :gene-list []),
          dialog-state (atom {:dialog-result nil,
                              :group-name nil,
                              :group-data empty-group-data}),
          plot (ref nil),
          control (jfx/create-control "avatar/ui/GeneSelectionDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [remove-genes-button,
                  ^ListView
                  selection-listview,
                  ^StackPane
                  plot-parent,
                  ^Label
                  selected-label,
                  ^ComboBox
                  alteration-type-combobox,
                  add-gene-button,
                  add-gene-list-button,
                  clear-button,
                  set-group-button,
                  move-top-button
                  fix-button,
                  filter-button,
                  remove-button,
                  select-button,
                  close-button]
           :as children} (jfx/control-children control)
          window (doto (jfx/modal-window "Select genes", control-node)
                   (t/add-avatar-icon)),
          ; setup alteration type choice
          alteration-type-prop (t/setup-alteration-type-combobox alteration-type-combobox, alteration-type-set,
                                 :selected-alteration-type shown-alteration-type,
                                 :alteration-type-change
                                 (fn [alteration-type]
                                   (sort-samples dialog-state, alteration-type)))]

      ; enable multiple selection
      (jfx-select/enable-multiple-selection! selection-listview)

      (jfx/bind (jfx/property selected-label, :text)
        (jfx/functional-property
          (fn [gene-list]
            (format "%d selected genes" (count gene-list)))
          (jfx/map-entry-property dialog-state, [:group-data, :gene-list])))

      ; handler to remove genes
      (jfx/handle-event! remove-genes-button, :action,
        (fn [_]
          (update-group-data-based-on-selection dialog-state, gene-list, selection-listview,
            (partial remove-genes (jfx/value alteration-type-prop)))))

      ; bind :group-data :gene-list to group-genes-listview
      (jfx/bind (jfx/property selection-listview, :items)
        (jfx/functional-property
          (fn [gene-list]
            (jfx/observable-array-list (mapv :gene gene-list)))
          (jfx/map-entry-property dialog-state, [:group-data, :gene-list])))

      ; intialize plot when window is shown
      (jfx/handle-event! window, :window-shown,
        (fn [_]
          (let [[width, height] (jfx/layout-bounds plot-parent),
                plot (plot/create-plot empty-group-data, plot-style, width, height,
                       {:plot-ref plot,
                        :show-header? false,
                        :snapshot-handler (partial pexp/save-plot-as-snapshot (:data-management ui)),
                        :show-objectives-handler (partial sv/show-objectives-view (:data-management ui)),})]
            (jfx/add-child plot-parent, (plot/root-control plot)))))

      (jfx/listen-to
        (fn [^Bounds layout-bounds]
          (plot/resize-plot plot, (.getWidth layout-bounds), (.getHeight layout-bounds)))
        (jfx/property plot-parent, :layout-bounds))

      (jfx/listen-to
        (fn [gene-sample-data, alteration-type]
          (plot/update-data! plot, gene-sample-data, (assoc plot-style :shown-alteration-type alteration-type)))
        (jfx/map-entry-property dialog-state [:group-data])
        alteration-type-prop)


      (jfx/handle-event! add-gene-button, :action,
        (fn [_]
          (find+add-gene ui, dialog-state, (jfx/value alteration-type-prop), gene-list)))

      (jfx/handle-event! add-gene-list-button, :action,
        (fn [_]
          (find+add-genes-from-list ui, dialog-state, (jfx/value alteration-type-prop), gene-list)))


      (jfx/handle-event! clear-button, :action,
        (fn [_]
          (swap! dialog-state assoc-in [:group-data, :gene-list] [])))


      ; gene action buttons

      (doseq [[^Button button, action] [[set-group-button, :set-gene-group]
                                        [move-top-button, :move-genes-to-top]
                                        [fix-button, :fix-genes]
                                        [filter-button, :filter-genes]
                                        [remove-button, :remove-genes]
                                        [select-button, :select-genes]]]
        ; disable when no gene selected
        (jfx/bind (jfx/property button, :disable)
          (jfx/functional-property
            empty?
            (jfx/map-entry-property dialog-state, [:group-data, :gene-list])))

        ; button handler
        (jfx/handle-event! button, :action,
          (fn [_]
            (let [gene-list (get-in (deref dialog-state) [:group-data, :gene-list])]
              (when (seq gene-list)
                (complete-fn {:action action, :gene-list gene-list})
                ; need to close, because the gene list processed in "application" is wrong after the first action
                ; do we need multiple edits here?
                (jfx/close window))))))




      (jfx/handle-event! close-button, :action
        (fn [_]
          (jfx/close window)))

      (jfx/show window))))