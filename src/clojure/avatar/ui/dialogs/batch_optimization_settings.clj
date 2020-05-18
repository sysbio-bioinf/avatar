; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.batch-optimization-settings
  (:require [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [avatar.ui.dialogs.single-optimization-settings :as single-opt-set]
            [clj-jfx.selection :as jfx-select]
            [avatar.data.gene-data :as gd]
            [avatar.ui.tools :as t]
            [clojure.string :as str])
  (:import (javafx.scene.control Button CheckBox TextField Label)
           (javafx.scene.text Text)))




(defn snapshot-alteration-types
  [snapshot-list]
  (transduce
    (map #(get-in % [:data, :alteration-type-set]))
    (completing u/into! persistent!)
    (transient #{})
    snapshot-list))


(defn snapshot-sample-group-map
  [snapshot-list, snapshot-id-set]
  (transduce
    (comp
      ; filter by :id
      (filter
        (fn [{:keys [id]}]
          (contains? snapshot-id-set id)))
      ; extract sample groups with snapshot reference
      (mapcat
        (fn [{:keys [name, data]}]
          (->> data
            gd/sample-groups
            (mapv
              #(hash-map :snapshot name, :sample-group %))))))
    (completing
      (fn [result-map, {:keys [sample-group, snapshot] :as sample-group-map}]
        (u/update-in! result-map [sample-group] (fnil conj []) snapshot))
      persistent!)
    (transient {})
    snapshot-list))


(defn update-feasible-snapshots
  [feasible-snapshots, snapshot-list, alteration-type]
  (when alteration-type
    (let [id->feasible-snapshot (zipmap (mapv :id feasible-snapshots) feasible-snapshots)]
      (->> snapshot-list
        (into []
          (comp
            ; find snapshots with given alteration type
            (filter
              (fn [snapshot]
                (contains? (get-in snapshot [:data, :alteration-type-set]) alteration-type)))
            ; extract :id and :name, rese previous :optimize? value if possible
            (map
              (fn [{:keys [id] :as snapshot}]
                (-> snapshot
                  (select-keys [:id, :name])
                  (assoc :optimize? (if-let [{:keys [optimize?]} (get id->feasible-snapshot id)]
                                      optimize?
                                      false)))))))
        (sort-by :name)
        vec))))


(defn update-available-sample-groups
  [available-sample-groups, snapshot-list, selected-snapshots]
  (when (seq selected-snapshots)
    (let [name->sample-group (zipmap (mapv :sample-group available-sample-groups) available-sample-groups)]
      (->> (snapshot-sample-group-map snapshot-list, (into #{} (map :id) selected-snapshots))
        (mapv
          (fn [[sample-group, snapshots]]
            {:sample-group sample-group,
             :snapshots snapshots,
             :snapshots-text (->> snapshots sort (str/join ", ")),
             :optimize? (if-let [{:keys [optimize?]} (get name->sample-group sample-group)]
                          optimize?
                          false)}))))))


(def second-objective->text-map {:overlap "overlap", :gene-count "gene count"})

(defn show-edit-optimization-dialog
  [snapshot-list, optimization-setup, complete-fn]
  (jfx/run-now
    (let [alteration-types (gd/sort-alteration-types (snapshot-alteration-types snapshot-list))
          dialog-state (atom {:alteration-types alteration-types,
                              :second-objectives [:overlap, :gene-count]
                              :minimum-alteration-ratio 0.0})
          control (jfx/create-control "avatar/ui/BatchOptimizationSetupDialog.fxml")
          control-node (jfx/control-node control)
          {:keys [alteration-type-combobox,
                  second-objective-combobox,
                  ^CheckBox
                  all-samples-checkbox,
                  snapshot-tableview
                  sample-group-tableview
                  ^CheckBox
                  select-all-groups-checkbox
                  ^Text
                  optimization-count-text
                  minimum-alterations-spinner
                  ^Button
                  apply-button
                  cancel-button]} (jfx/control-children control)
          window (doto (jfx/modal-window
                         (if optimization-setup "Edit optimization setup" "Add optimization setup"),
                         control-node)
                   (t/add-avatar-icon)),
          selected-alteration-type-prop (jfx/map-entry-property dialog-state, [:selected-alteration-type])
          feasible-snapshots-prop (jfx/map-entry-property dialog-state, [:feasible-snapshots])
          available-sample-groups-prop (jfx/map-entry-property dialog-state, [:available-sample-groups])]

      ; alteration type
      (jfx/combobox-bidi alteration-type-combobox,
        (jfx/map-entry-property dialog-state, [:alteration-types]),
        selected-alteration-type-prop,
        (zipmap alteration-types (mapv gd/alteration-type-str alteration-types)))

      ; second objective
      (jfx/combobox-bidi second-objective-combobox,
        (jfx/map-entry-property dialog-state, [:second-objectives])
        (jfx/map-entry-property dialog-state, [:selected-second-objective])
        second-objective->text-map)

      ; alteration rate
      (jfx/spinner-bidi minimum-alterations-spinner, :double, 0.0, 1.0, 0.01,
        (jfx/map-entry-property dialog-state, [:minimum-alteration-ratio]))

      ; optimization on all samples?
      (jfx/bind-bidirectional
        (jfx/map-entry-property dialog-state, [:optimization-on-all-samples?]),
        (jfx/property all-samples-checkbox, :selected))

      ; determine feasible snapshots
      (jfx/bind-updating feasible-snapshots-prop, selected-alteration-type-prop,
        (fn [feasible-snapshots, _, alteration-type]
          (update-feasible-snapshots feasible-snapshots, snapshot-list, alteration-type)))

      ; setup snapshot table
      (jfx/setup-table-view! snapshot-tableview, [:optimize?, :name], dialog-state,
        :data-prefix [:feasible-snapshots]
        :sort? true
        :column-cell-factories
        {:optimize? (fn [] (jfx/checkbox-cell))})

      ; keep track of selected snapshots
      (jfx/bind (jfx/map-entry-property dialog-state [:selected-snapshots])
        (jfx/functional-property
          (fn [feasible-snapshots]
            (into #{} (filter :optimize?) feasible-snapshots))
          feasible-snapshots-prop))

      ; determine sample groups in :selected-snapshots
      (jfx/bind-updating available-sample-groups-prop, (jfx/map-entry-property dialog-state, [:selected-snapshots])
        (fn [available-sample-groups, _, selected-snapshots]
          (update-available-sample-groups available-sample-groups, snapshot-list, selected-snapshots)))


      ; setup sample group table
      (jfx/setup-table-view! sample-group-tableview, [:optimize?, :sample-group, :snapshots-text], dialog-state,
        :data-prefix [:available-sample-groups]
        :sort? true
        :column-cell-factories
        {:optimize? (fn [] (jfx/checkbox-cell))})

      ; keep track of selected sample groups
      (jfx/bind (jfx/map-entry-property dialog-state, [:selected-sample-groups])
        (jfx/functional-property
          (fn [available-sample-groups]
            (into #{} (filter :optimize?) available-sample-groups))
          available-sample-groups-prop))


      ; select-all-groups-checkbox updates :optimize? attribute of all rows
      (jfx/listen-to
        (fn [selected?]
          (swap! dialog-state update-in [:available-sample-groups]
            (fn [rows]
              (mapv
                #(assoc % :optimize? selected?)
                rows))))
        (jfx/property select-all-groups-checkbox, :selected))

      ; if checkbox selected but not every row has :optimize? true, then set checkbox to :indeterminate
      (jfx/listen-to
        (fn [rows]
          (when (jfx/property-value select-all-groups-checkbox, :selected)
            (let [all-selected? (every? :optimize? rows)
                  non-selected? (not-any? :optimize? rows)]
              (jfx/property-value! select-all-groups-checkbox, :indeterminate (and (not all-selected?) (not non-selected?)))
              (when non-selected?
                (jfx/property-value! select-all-groups-checkbox, :selected false)))))
        (jfx/map-entry-property dialog-state, [:available-sample-groups]))

      ; determine the number of optimization runs
      (let [optimization-run-prop (jfx/functional-property
                                    (fn [selected-sample-groups, optimization-on-all-samples?, selected-snapshots]
                                      (transduce
                                        (map (comp count :snapshots))
                                        +
                                        (if optimization-on-all-samples?
                                          (count selected-snapshots)
                                          0)
                                        selected-sample-groups))
                                    (jfx/map-entry-property dialog-state, [:selected-sample-groups])
                                    (jfx/map-entry-property dialog-state, [:optimization-on-all-samples?])
                                    (jfx/map-entry-property dialog-state, [:selected-snapshots]))]
        (jfx/bind (jfx/map-entry-property dialog-state, :optimization-run-count) optimization-run-prop)
        (jfx/bind (jfx/property optimization-count-text, :text) (jfx/functional-property str optimization-run-prop)))

      (let [required-attributes [:selected-alteration-type, :selected-second-objective, :selected-snapshots, :selected-sample-groups, :optimization-on-all-samples?]]
        (jfx/bind (jfx/property apply-button, :disable),
          (jfx/functional-property
            (fn [{:keys [selected-alteration-type, selected-second-objective, selected-snapshots,
                         selected-sample-groups, optimization-on-all-samples?]}]
              (not
                (and
                  selected-alteration-type
                  selected-second-objective,
                  (seq selected-snapshots)
                  (or optimization-on-all-samples? (seq selected-sample-groups)))))
            (jfx/map-property dialog-state, required-attributes)))

        (jfx/handle-event! apply-button, :action,
          (fn [_]
            (complete-fn (deref dialog-state))
            (jfx/close window))))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      (when optimization-setup
        (swap! dialog-state merge optimization-setup))

      (jfx/show window))))


(defn add-optimization-setup
  [snapshot-list, dialog-state]
  (show-edit-optimization-dialog snapshot-list, nil,
    (fn [optimization-setup]
      (swap! dialog-state update-in [:batch, :optimization-setup-list] conj optimization-setup))))


(defn optimization-setup-selected?
  [selected-optimization-setup]
  (and selected-optimization-setup (<= 0 selected-optimization-setup)))


(defn edit-optimization-setup
  [snapshot-list, dialog-state]
  (let [{:keys [selected-optimization-setup, optimization-setup-list]} (:batch (deref dialog-state))]
    (when (optimization-setup-selected? selected-optimization-setup)
      (show-edit-optimization-dialog snapshot-list, (nth optimization-setup-list selected-optimization-setup),
        (fn [optimization-setup]
          (swap! dialog-state update-in [:batch, :optimization-setup-list] assoc selected-optimization-setup optimization-setup))))))


(defn delete-optimization-setup
  [dialog-state]
  (swap! dialog-state update-in [:batch]
    (fn [{:keys [selected-optimization-setup] :as batch}]
      (when (optimization-setup-selected? selected-optimization-setup)
        (update-in batch [:optimization-setup-list] (partial u/delete #{selected-optimization-setup}))))))


(defn minimal-gene-count
  [snapshot-list]
  (transduce
    (map (comp count #(get-in % [:data, :gene-list])))
    (fn min-nil-safe
      ([a] a)
      ([a, b]
       (if a
         (min a b)
         b)))
    nil
    snapshot-list))

; TODO: conversion from batch-config to single-run
;; individual runs are single threaded
;:thread-count 1,
;:exclude-fixed-rows? true,
;:show-progress? true


(defn extract-config
  [dialog-state]
  (select-keys (deref dialog-state) [:batch, :expert, :simple, :setup-type]))


(defn save-config
  [window, batch-config]
  (when-let [file (jfx/choose-file window, "Save batch config", :save,
                    {:file-types {"Batch config file (*.batch)" ["*.batch"]}})]
    (spit file, (pr-str batch-config))))


(defn show-dialog
  [snapshot-list, prev-settings, complete-fn]
  (jfx/run-now
    (let [min-gene-count (minimal-gene-count snapshot-list)
          initial-state (u/deep-merge
                          {:gene-count min-gene-count,
                           :batch {:optimization-setup-list []
                                   :thread-count (u/cpu-thread-count)}}
                          (single-opt-set/initial-parameter-config min-gene-count)
                          prev-settings),
          dialog-state (atom initial-state),
          control (jfx/create-control "avatar/ui/BatchOptimizationSettingsDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [thread-count-combobox
                  add-optimization-setup-button
                  ^Button
                  edit-optimization-setup-button
                  ^Button
                  delete-optimization-setup-button
                  optimization-setup-tableview
                  ^TextField
                  batch-name-textfield
                  ^Label
                  batch-name-error-label,
                  ^Label
                  table-error-label
                  ^Text
                  optimization-count-text
                  ^Button
                  run-button
                  ^Button
                  save-config-button
                  cancel-button]
           :as control-map} (jfx/control-children control),
          window (doto (jfx/modal-window "Gene Selection Optimization Setup", control-node)
                   (t/add-avatar-icon))
          max-thread-count (u/cpu-thread-count)
          selected-optimization-setup-prop (jfx/map-entry-property dialog-state [:batch, :selected-optimization-setup])
          batch-name-prop (jfx/map-entry-property dialog-state [:batch, :name])
          optimization-setup-list-prop (jfx/map-entry-property dialog-state, [:batch, :optimization-setup-list])]

      ; thread count choice
      (single-opt-set/setup-combobox thread-count-combobox, dialog-state [:batch, :thread-count],
        #(- max-thread-count %),
        #(- max-thread-count %),
        (mapv inc (reverse (range max-thread-count))))

      ; algorithm parameters
      (single-opt-set/setup-algorithm-parameters-control control-map, initial-state, dialog-state)

      ; selected optimization setup
      (jfx/bind selected-optimization-setup-prop
        (jfx-select/selected-index-property optimization-setup-tableview))

      ; batch name
      (jfx/bind-bidirectional (jfx/property batch-name-textfield, :text) batch-name-prop)

      ; setup table
      (let [optimization-setup-table-data (atom nil)]
        (jfx/listen-to
          (fn [optimization-setup-list]
            (reset! optimization-setup-table-data
              (mapv
                (fn [{:keys [selected-alteration-type, selected-second-objective, selected-snapshots, selected-sample-groups,
                             minimum-alteration-ratio, optimization-on-all-samples?]}]
                  {:alteration-type (gd/alteration-type-str selected-alteration-type),
                   :second-objective (get second-objective->text-map selected-second-objective)
                   :minimum-alteration-ratio (format "%.2f" minimum-alteration-ratio)
                   :snapshots-text (->> selected-snapshots
                                     (mapv :name)
                                     sort
                                     (str/join ", ")),
                   :sample-groups-text (cond->
                                         (->> selected-sample-groups
                                           (mapv :sample-group)
                                           sort
                                           (str/join ", "))
                                         optimization-on-all-samples? (str ", ALL SAMPLES"))})
                optimization-setup-list)))
          optimization-setup-list-prop)

        (jfx/setup-table-view! optimization-setup-tableview,
          [:alteration-type, :second-objective, :minimum-alteration-ratio, :snapshots-text, :sample-groups-text]
          optimization-setup-table-data))


      (jfx/handle-event! add-optimization-setup-button, :action,
        (fn [_]
          (add-optimization-setup snapshot-list, dialog-state)))


      ; edit setup button
      (jfx/bind (jfx/property edit-optimization-setup-button, :disable)
        (jfx/functional-property (complement optimization-setup-selected?)
          selected-optimization-setup-prop))

      (jfx/handle-event! edit-optimization-setup-button, :action,
        (fn [_]
          (edit-optimization-setup snapshot-list, dialog-state)))


      ; delete setup button
      (jfx/bind (jfx/property delete-optimization-setup-button, :disable)
        (jfx/functional-property (complement optimization-setup-selected?)
          selected-optimization-setup-prop))

      (jfx/handle-event! delete-optimization-setup-button, :action
        (fn [_]
          (delete-optimization-setup dialog-state)))


      ; validation
      (let [batch-name-error-prop (jfx/functional-property
                                    (fn [batch-name]
                                      (when (str/blank? batch-name)
                                        "Enter a batch name."))
                                    batch-name-prop)
            total-optimization-run-count-prop (jfx/functional-property
                                                (fn [optimization-setup-list]
                                                  (transduce
                                                    (map :optimization-run-count)
                                                    (completing +)
                                                    optimization-setup-list))
                                                optimization-setup-list-prop)
            optimization-setups-error-prop (jfx/functional-property
                                             (fn [total-optimization-run-count]
                                               (when (zero? total-optimization-run-count)
                                                 "You need to specify at least one optimization setup."))
                                             total-optimization-run-count-prop)
            invalid-settings? (jfx/functional-property
                                (fn [expert-error?, batch-name-error, optimization-setups-error]
                                  (or expert-error? (boolean batch-name-error) (boolean optimization-setups-error)))
                                (single-opt-set/expert-parameters-error?-prop dialog-state)
                                batch-name-error-prop
                                optimization-setups-error-prop)]

        (jfx/bind (jfx/property optimization-count-text, :text)
          (jfx/functional-property str total-optimization-run-count-prop))

        ; error text
        (jfx/bind (jfx/property batch-name-error-label, :text) batch-name-error-prop)
        (jfx/bind (jfx/property table-error-label, :text) optimization-setups-error-prop)

        ; bind run-button disable
        (jfx/bind (jfx/property run-button, :disable) invalid-settings?)
        ; bin save-config-button disable
        (jfx/bind (jfx/property save-config-button, :disable) invalid-settings?))


      (jfx/handle-event! run-button, :action,
        (fn [_]
          (jfx/close window)
          (complete-fn (extract-config dialog-state))))


      (jfx/handle-event! save-config-button, :action,
        (fn [_]
          (when (save-config window, (extract-config dialog-state))
            (jfx/close window))))

      (jfx/handle-event! cancel-button, :action, (fn [_] (jfx/close window)))
      (jfx/show window))))
