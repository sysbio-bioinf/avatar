; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.drag-and-drop
  (:require [avatar.ui.plot.data :as pdata]
            [avatar.ui.plot.selection :as select]
            [avatar.util :as u]
            [clj-jfx.core :as jfx]
            [clj-jfx.colors :as col])
  (:import (javafx.scene.input MouseEvent TransferMode DragEvent ClipboardContent DataFormat MouseButton)
           (javafx.scene Node)
           (javafx.scene.control ToggleButton)
           (java.util UUID)
           (javafx.event Event)))



; use a random UUID to be able to develop in repl (DataFormat can be defined only once and cannot be removed afterwards)
(defonce clojure-data-dataformat (DataFormat. (into-array String [(str "Clojure Data-" (UUID/randomUUID))])))


(defn clojure-clipboard
  ^ClipboardContent [data]
  (doto (ClipboardContent.)
    (.put clojure-data-dataformat, data)))


(defn drag-and-drop-data
  [^DragEvent drag-event]
  (let [dragboard (.getDragboard drag-event)]
    (.getContent dragboard clojure-data-dataformat)))


(defn header-view-index->data-index
  [plot, header-type, header-index]
  (let [{:keys [view]} (:state (deref plot))]
    (pdata/header-view-index->data-index view, header-type, header-index)))


(defn drag-detected
  [plot, header-type, header-index, ^MouseEvent drag-event]
  (when (= (.getButton drag-event) MouseButton/PRIMARY)
    (let [^ToggleButton source (.getSource drag-event)]
      ; start drag only for selected toggle buttons
      (if (.isSelected source)
        (let [; drag-board currently not needed, only startDragAndDrop
              drag-board (.startDragAndDrop source (into-array [TransferMode/MOVE]))]
          ; need to add content to drag-board to initiate drag and drop
          (.setContent drag-board (clojure-clipboard {:action :move, :source-header-index header-index, :source-header-type header-type})))
        (let [; drag-board currently not needed, only startDragAndDrop
              drag-board (.startDragAndDrop source (into-array [TransferMode/LINK]))]
          (.setContent drag-board (clojure-clipboard {:action :select, :source-header-index header-index, :source-header-type header-type}))))))
  (.consume drag-event))


(defn fixed-row?
  [plot, header-index]
  (let [{{:keys [plot-data, view]} :state} (deref plot)]
    (pdata/fixed-row? plot-data, (pdata/header-view-index->data-index view, :row, header-index))))



(defn highlight-selection-range
  [plot, header-type, view-from-index, view-to-index]
  (let [{:keys [controls, state]} (deref plot),
        {:keys [plot-style, plot-data, view]} state,
        {:keys [selection-color, fixed-row-header-color]} plot-style,
        headers (get-in controls [:headers, header-type, :headers])
        [view-from-index, view-to-index] (if (<= view-from-index view-to-index)
                                           [view-from-index, view-to-index]
                                           [view-to-index, view-from-index]),
        color-str (fn [color]
                    (format "-fx-base: %s;" (col/color->str color)))
        fixed-row? (fn [index]
                     (and
                       (= header-type :row)
                       (pdata/fixed-row? plot-data, (pdata/header-view-index->data-index view, header-type index))))]
    (u/for-each-indexed!
      (fn [view-index, ^Node header]
        (.setStyle header
          (cond
            (<= view-from-index view-index view-to-index) (color-str selection-color)
            (fixed-row? view-index) (color-str fixed-row-header-color))))
      headers)))


(defn drag-over
  [plot, target-header-type, target-header-index, ^DragEvent drag-event]
  (let [source (.getGestureSource drag-event),
        target (.getTarget drag-event),
        target (if (instance? ToggleButton target)
                 target
                 (.getParent ^Node target)),
        {:keys [source-header-index, source-header-type, action]} (drag-and-drop-data drag-event)]
    #_(println "drag-over" (name target-header-type) target-header-index)
    (when (and
            (= source-header-type target-header-type)
            (or (not= source-header-type :row) (= (fixed-row? plot, source-header-index) (fixed-row? plot, target-header-index))))
      (when (= action :select)
        (highlight-selection-range plot, target-header-type, source-header-index, target-header-index))
      (when (or (= action :select) (not= source target))
        (.acceptTransferModes drag-event (into-array [TransferMode/MOVE, TransferMode/LINK]))))
    (.consume drag-event)))


(defn drag-entered
  [plot, header-type, header-index, ^DragEvent drag-event]
  (.consume drag-event))


(defn drag-exited
  [plot, header-type, header-index, ^DragEvent drag-event]
  (.consume drag-event))


(defn drag-dropped
  [plot, target-header-type, target-header-index, ^DragEvent drag-event]
  (let [target-data-header-index (header-view-index->data-index plot, target-header-type, target-header-index),
        {:keys [action] :as dnd-data} (drag-and-drop-data drag-event)]
    (case action
      :move (let [dnd-handler (pdata/action-handler plot, :drag-and-drop-handler)]
              (when dnd-handler
                (dnd-handler plot, target-header-type, (pdata/selected-headers plot, target-header-type), target-data-header-index)
                (select/clear-header-selection! plot)))
      :select (let [{:keys [source-header-index]} dnd-data,
                    source-data-header-index (header-view-index->data-index plot, target-header-type, source-header-index)]
                (highlight-selection-range plot, target-header-type, -1, -1)
                (select/select-headers-from-to plot, target-header-type, source-data-header-index, target-data-header-index)))
    (.setDropCompleted drag-event true)
    (.consume drag-event)))


(defn drag-done
  [plot, header-type, header-index, ^DragEvent drag-event]
  (let [{:keys [action, source-header-type]} (drag-and-drop-data drag-event)]
    (when (= action :select)
      (highlight-selection-range plot, source-header-type, -1, -1)))
  (.consume drag-event))


(defonce drag-and-drop-handler
  {MouseEvent/DRAG_DETECTED drag-detected,
   DragEvent/DRAG_OVER drag-over,
   DragEvent/DRAG_ENTERED drag-entered,
   DragEvent/DRAG_EXITED drag-exited,
   DragEvent/DRAG_DROPPED drag-dropped,
   DragEvent/DRAG_DONE drag-done})


(defn on-drag-and-drop
  [^Event event, plot, header-type, header-index]
  (when-let [handler (get drag-and-drop-handler (.getEventType event))]
    (handler plot, header-type, header-index, event)))