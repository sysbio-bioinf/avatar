; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.data
  (:require [avatar.util :as u])
  (:import (javafx.event Event)
           (javafx.scene.control ContextMenu)))




(defn data
  [plot]
  (-> plot deref :state :plot-data))


(defn style
  [plot]
  (-> plot deref :state :plot-style))


(defn style!
  [plot, style]
  (dosync
    (doto plot
      (alter assoc-in [:state, :plot-style] style))))


(defn missing-event-handler
  [event-handler-kw, ^Event event & args]
  (u/runtime-exception "Missing event handler for %s! Please register an event handler for this event via %s."
    (-> event .getEventType .getName), event-handler-kw))


(defn action-handler
  [plot, action-handler-kw]
  (get-in (deref plot) [:action-handler-map, action-handler-kw]))


(defn context-menu
  ^ContextMenu [plot, type]
  (get-in (deref plot) [:controls, :context-menu-map, type]))


(defn contextmenu-data!
  [plot, data-map]
  (dosync (alter plot assoc :context-menu-data data-map))
  plot)


(defn contextmenu-data
  [plot]
  (:context-menu-data (deref plot)))


(defn fixed-gene?
  [gene]
  (boolean (:order-fixed? gene)))


(defn fixed-row?
  [{:keys [gene-list] :as plot-data}, gene-index]
  (fixed-gene? (nth gene-list gene-index)))


(defn gene-group
  [{:keys [group, gene]}]
  (or group gene))


(defn group-map
  [{:keys [gene-list, sample-count, sample-permutation, sample-group-map] :as plot-data}]
  (let [row-groups (->> gene-list
                     (map-indexed
                       (fn [index, gene]
                         {:effective-group (gene-group gene),
                          :group (:group gene)
                          :index index}))
                     (partition-by :effective-group)
                     (mapv
                       (fn [gene-entries]
                         {:indices (mapv :index gene-entries),
                          :group (-> gene-entries first :group)}))),
        {:keys [column->sample]} sample-permutation,
        column-groups (->> (range sample-count)
                        (map
                          (fn [index]
                            {:group (let [sample (get column->sample index)]
                                      (get sample-group-map sample))
                             :index index}))
                        (partition-by :group)
                        (mapv
                          (fn [sample-entries]
                            {:indices (mapv :index sample-entries)
                             :group (-> sample-entries first :group)})))]
    {:row row-groups
     :column column-groups}))


(defn headers
  [controls-map, header-type]
  (get-in controls-map [:headers, header-type, :headers]))


(defn header-view-index->data-index
  [{:keys [first-gene-row-index, first-sample-column-index] :as view}, header-type, index]
  (case header-type
    :row (+ index first-gene-row-index)
    :column (+ index first-sample-column-index)))


(defn header-data-index->view-index
  [{:keys [first-gene-row-index, first-sample-column-index] :as view}, header-type, index]
  (case header-type
    :row (- index first-gene-row-index)
    :column (- index first-sample-column-index)))


(defn selected-headers
  [plot, header-type]
  (get-in (deref plot) [:selection :selected-header-map, header-type]))


(defn overlay-pane
  [plot]
  (get-in (deref plot) [:controls, :overlay-pane]))


(defn header-selected?
  [plot, header-type, header-view-index]
  (dosync
    (let [{:keys [state, selection]} (deref plot),
          header-data-index (header-view-index->data-index (:view state), header-type, header-view-index)
          selected-set (get-in selection [:selected-header-map, header-type])]
      (contains? selected-set header-data-index))))