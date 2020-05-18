; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.modification
  (:require
    [avatar.ui.plot.data :as pdata]
    [clj-jfx.core :as jfx]
    [avatar.ui.plot.structure :as struct]
    [avatar.ui.plot.selection :as select]
    [clj-jfx.colors :as col]
    [avatar.ui.plot.util :as pu]
    [avatar.ui.tools :as t])
  (:import (javafx.scene.control ComboBox Button TextField)
           (java.util Collection)
           (javafx.event Event)
           (javafx.scene.layout Pane Region)
           (javafx.scene Node)))



(defn on-modify-fixed-order
  [fix?, plot, header-type, event]
  (let [selected-rows (pdata/selected-headers plot, header-type)]
    (when-let [fix-rows-handler (pdata/action-handler plot, :fix-rows-handler)]
      (fix-rows-handler selected-rows, fix?))))


(defn on-move-to-front
  [plot, header-type, event]
  (let [selection (pdata/selected-headers plot, header-type)]
    (when-let [move-to-front-handler (pdata/action-handler plot, :move-to-front-handler)]
      (move-to-front-handler header-type, selection))))


(defn selected-gene-groups
  [plot, selected-header-indices]
  (let [{:keys [gene-list]} (get-in (deref plot) [:state, :plot-data])]
    (->> selected-header-indices
      (keep
        (fn [gene-index]
          (:group (nth gene-list gene-index))))
      vec)))


(defn selected-sample-groups
  [plot, selected-header-indices]
  (let [{:keys [sample-group-map, sample-permutation]} (get-in (deref plot) [:state, :plot-data])
        {:keys [column->sample]} sample-permutation]
    (->> selected-header-indices
      (keep
        (fn [sample-column-index]
          (let [sample-index (nth column->sample sample-column-index)]
            (get sample-group-map sample-index))))
      vec)))


(defn ordered-by-frequency
  [groups]
  (->> groups
    frequencies
    (sort-by val >)
    (mapv key)))


(defn on-change-group
  [plot, header-type, event]
  (when-let [multi-group-edit-handler (pdata/action-handler plot, :multi-group-edit-handler)]
    (let [selected-header-indices (pdata/selected-headers plot, header-type),
          group-editor (jfx/load-control "avatar/ui/GroupEditor.fxml"),
          window (doto (jfx/modal-window "Change Group", group-editor)
                   (t/add-avatar-icon)),
          groups (case header-type
                   :row (selected-gene-groups plot, selected-header-indices)
                   :column (selected-sample-groups plot, selected-header-indices)),
          group-combobox (doto ^ComboBox (jfx/find-node group-editor "group-combobox")
                           (-> .getItems (.addAll ^Collection (ordered-by-frequency groups)))
                           (-> .getSelectionModel (.select 0)))
          change-button-action (fn [^Event e]
                                 (let [new-group (.getValue group-combobox)]
                                   (.consume e)
                                   (.close window)
                                   (when multi-group-edit-handler
                                     (multi-group-edit-handler header-type, selected-header-indices, new-group)))),
          cancel-button-action (fn [^Event e]
                                 (.consume e)
                                 (.close window))]
      (jfx/event-handling! group-editor
        [["change-button", :action, change-button-action]
         ["cancel-button", :action, cancel-button-action]])
      (jfx/show window)
      ; select all text of combobox
      (doto (.getEditor group-combobox)
        (jfx/request-focus)
        (jfx/select-all-text)))))


(defn on-delete
  [plot, header-type, event]
  (let [selected-rows (pdata/selected-headers plot, header-type)]
    (when-let [delete-genes-handler (pdata/action-handler plot, :delete-genes-handler)]
      (delete-genes-handler header-type, selected-rows))))


(defn create-text-editor
  ^Region [x, y, width, height, text-value]
  (let [editor (doto ^Region (jfx/load-control "avatar/ui/LabelEditor.fxml")
                 (.setLayoutX x)
                 (.setLayoutY y)
                 (.setMinSize width, height)
                 (.setPrefSize width, height)
                 (.setMaxSize width, height))]
    (doseq [button-id ["accept-button", "cancel-button"]]
      (jfx/padding! ^Button (jfx/find-node editor, button-id)
        5.0))
    (.setText ^TextField (jfx/find-node editor, "content-textfield")
      text-value)
    editor))


(defn text-editor
  [{:keys [x, y] :as position}, ^Node target, text-value, additional-width, minimal-width]
  (let [[width, height] (jfx/layout-bounds target),
        width (max (+ width additional-width) minimal-width),]
    (create-text-editor x, y, width, height, text-value)))


(defn show-editor!
  [^Pane overlay-pane, ^Region editor, show?]
  (if show?
    (do
      (doto overlay-pane
        (jfx/add-child editor)
        (jfx/mouse-transparent! false)
        (jfx/background-color! (col/color "#BBBBBB", 0.5)))
      (doto ^TextField (jfx/find-node editor, "content-textfield")
        .requestFocus
        .selectAll)
      overlay-pane)
    (doto overlay-pane
      (jfx/remove-children editor)
      (jfx/mouse-transparent! true)
      (jfx/background-color! nil))))


(defn disable-ui!
  [plot, disable?]
  (when-let [disable-ui-fn (pdata/action-handler plot :disable-ui!)]
    (disable-ui-fn disable?))
  plot)


(defn handle-text-edit!
  [editor, handler-fn]
  (let [^TextField text-field (jfx/find-node editor, "content-textfield"),
        old-text-value (.getText text-field),
        accept-button-action (fn [^Event e]
                               (let [new-text-value (.getText text-field)]
                                 (handler-fn
                                   (if (= new-text-value old-text-value) :accept-unchanged :accept-modified),
                                   new-text-value)
                                 (.consume e))),
        cancel-button-action (fn [^Event e]
                               (handler-fn :cancel, old-text-value)
                               (.consume e))]
    (jfx/event-handling! editor,
      [["accept-button", :action, accept-button-action]
       ["cancel-button", :action, cancel-button-action]])))


(defn text-editor-position
  [{:keys [row-header-width,
           column-header-height,
           alteration-row-height,
           alteration-cell-width,
           sample-group-height,
           gene-group-width,
           gene-name-width] :as plot-layout},
   element-type,
   header-index]
  (case element-type
    :gene-group {:x row-header-width
                 :y (+ column-header-height sample-group-height (* header-index alteration-row-height))}
    :gene-name {:x (+ row-header-width gene-group-width)
                :y (+ column-header-height sample-group-height (* header-index alteration-row-height))}
    :sample-group {:x (+ row-header-width gene-group-width gene-name-width
                        (* header-index alteration-cell-width))
                   :y column-header-height}))


(defn text-cell-data
  [plot, element-type, element-index]
  (let [{{:keys [plot-data]} :state} (deref plot)]
    (if (= element-type :sample-group)
      (let [{:keys [sample-group-map, sample-permutation]} plot-data,
            {:keys [column->sample]} sample-permutation]
        (get sample-group-map (get column->sample element-index)))
      (when (#{:gene-group, :gene-name} element-type)
        (let [gene (get-in plot-data [:gene-list, element-index])]
          (case element-type
            :gene-group (:group gene)
            :gene-name (:gene gene)))))))


(defn on-text-cell-edit
  "Handle edit of sample or gene groups and gene names.
  For consistency with header-related functions, the type is called header-type
  and the corresponding row or column index is called header-index."
  [plot, header-type, header-index, cell-type, text-cell]
  (when-let [edit-handler-fn (pdata/action-handler plot, :single-cell-edit-handler)]
    (when (struct/textcell? text-cell)
      (let [overlay-pane (pdata/overlay-pane plot),
            {{:keys [view, plot-layout]} :state} (deref plot),
            element-type (case header-type
                           :column :sample-group,
                           :row (case cell-type
                                  :group :gene-group
                                  :gene :gene-name)),
            element-index (pdata/header-view-index->data-index view, header-type, header-index),
            editor (text-editor (text-editor-position plot-layout, element-type, header-index),
                     text-cell,
                     (text-cell-data plot, element-type, element-index),
                     47, 100)]
        (select/clear-header-selection! plot)
        (show-editor! overlay-pane, editor, true)
        (disable-ui! plot, true)
        (handle-text-edit! editor,
          (fn [result-mode, text-value]
            (show-editor! overlay-pane, editor, false)
            (disable-ui! plot, false)
            (edit-handler-fn element-type, element-index, result-mode, text-value)))))))