; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.gene-sort
  (:require [clj-jfx.core :as jfx]
            [clj-jfx.selection :as jfx-select]
            [avatar.util :as u]
            [avatar.ui.plot :as plot]
            [avatar.data.operations :as ops]
            [avatar.ui.plot-export :as pexp]
            [avatar.ui.dialogs.statistics-view :as sv]
            [avatar.ui.tools :as t])
  (:import (javafx.scene.control Slider Label RadioButton CheckBox TextField ComboBox)
           (javafx.geometry Bounds)
           (javafx.scene Node)
           (javafx.scene.text TextFlow)))


(def ^:const all-samples "All Samples")


(defn sample-groups
  [{:keys [sample-group-map] :as gene-sample-data}]
  (->> sample-group-map
    (reduce-kv
      (fn [result-set, _, group]
        (conj! result-set group))
      (transient #{}))
    persistent!
    sort
    (into [all-samples])))


(defn bind-sort-setting-controls
  [child-nodes, dialog-state, sort-settings-prefix]
  (let [{:keys [^Slider weighting-slider
                ^Label
                importance-info-label,
                ^TextField
                coverage-textfield
                ^RadioButton
                overlap-minimize-radiobutton
                ^RadioButton
                overlap-maximize-radiobutton
                ^CheckBox
                sort-by-group-checkbox
                ^CheckBox
                sort-by-sample-group-checkbox
                ^CheckBox
                exclude-fixed-genes-checkbox,
                ^CheckBox
                show-preview-checkbox
                ^TextFlow
                preview-note-textflow
                ^ComboBox
                sort-only-group-combobox,
                alteration-type-combobox]} child-nodes
        maybe-add-prefix (if sort-settings-prefix (partial into (u/->vector sort-settings-prefix)) identity)
        weighting-prop (jfx/map-entry-property dialog-state, (maybe-add-prefix [:weighting])),
        slider-value-prop (jfx/property weighting-slider, :value)
        toggle-group (jfx/toggle-group overlap-minimize-radiobutton, overlap-maximize-radiobutton)]
    ; slider
    (jfx/change-listener! weighting-prop
      (fn [_, old-weighting, new-weighting]
        (when-not (= old-weighting new-weighting)
          (jfx/value! slider-value-prop new-weighting))))
    (jfx/change-listener! slider-value-prop
      (fn [_, old-value, new-value]
        (when-not (= old-value new-value)
          (let [rounded-value (-> new-value (* 100.0) Math/round (/ 100.0))]
            (jfx/value! slider-value-prop, rounded-value)
            (jfx/value! weighting-prop, rounded-value)))))
    ; show weighting
    (jfx/bind (jfx/property importance-info-label, :text)
      (jfx/functional-property
        (fn [weighting]
          (format "Overlap: %.0f%%  -  Contribution: " (* 100.0 (- 1.0 weighting))))
        weighting-prop))
    ; coverage textfield
    (jfx/listen-to
      (fn [weighting]
        (jfx/property-value! coverage-textfield, :text, (->> weighting (* 100.0) (format "%.0f"))))
      weighting-prop)
    (jfx/property-value! coverage-textfield, :text, (->> (get-in (deref dialog-state) (maybe-add-prefix [:weighting])) (* 100.0) (format "%.0f")))
    (jfx/text-field-format! coverage-textfield (jfx/integer-format), #(<= 0 % 100))
    (jfx/listen-to
      (fn [weighting-text]
        (swap! dialog-state assoc-in
          (maybe-add-prefix [:weighting])
          (/ (Long/parseLong weighting-text) 100.0)))
      (jfx/property coverage-textfield, :text))
    ; overlap optimization goal
    (jfx/change-listener! (jfx/property toggle-group, :selected-toggle)
      (fn [_, old-selected-toggle, new-selected-toggle]
        (when-not (= old-selected-toggle new-selected-toggle)
          (swap! dialog-state assoc-in (maybe-add-prefix [:overlap])
            (if (= new-selected-toggle overlap-minimize-radiobutton)
              :minimize
              :maximize)))))
    (jfx/change-listener! (jfx/map-entry-property dialog-state, (maybe-add-prefix [:overlap]))
      (fn [_, old-overlap, new-overlap]
        (when-not (= old-overlap new-overlap)
          (.selectToggle toggle-group
            (case new-overlap
              :minimize overlap-minimize-radiobutton,
              :maximize overlap-maximize-radiobutton)))))
    ; sort by genes group
    (jfx/bind-bidirectional (jfx/property sort-by-group-checkbox, :selected), (jfx/map-entry-property dialog-state, (maybe-add-prefix [:by-group?])))
    ; sort by samples group
    (jfx/bind-bidirectional (jfx/property sort-by-sample-group-checkbox, :selected), (jfx/map-entry-property dialog-state, (maybe-add-prefix [:sample-group-first?])))
    ; ignore alterations of fixed genes
    (jfx/bind-bidirectional (jfx/property exclude-fixed-genes-checkbox, :selected), (jfx/map-entry-property dialog-state, (maybe-add-prefix [:exclude-fixed-rows?])))
    ; show preview?
    (when show-preview-checkbox
      (jfx/bind-bidirectional (jfx/property show-preview-checkbox, :selected), (jfx/map-entry-property dialog-state, (maybe-add-prefix [:show-preview?]))))
    (when preview-note-textflow
      (jfx/property-value! preview-note-textflow, :visible, (not (:show-preview? (deref dialog-state)))))
    (when sort-only-group-combobox
      ; add sample groups to combobox
      (jfx/property-value! sort-only-group-combobox, :items,
        (jfx/observable->observable-list
          (jfx/functional-property
            sample-groups
            (jfx/map-entry-property dialog-state, (maybe-add-prefix [:gene-sample-data])))))
      ; select first sample group
      (jfx/select-first! sort-only-group-combobox)
      ; bind selection to dialog state
      (jfx/bind (jfx/map-entry-property dialog-state, (maybe-add-prefix [:only-sample-group]))
        (jfx/functional-property
          (fn [selected-sample-group]
            (when-not (= selected-sample-group all-samples)
              selected-sample-group))
          (jfx-select/selected-item-property sort-only-group-combobox))))
    ; alteration type
    (when alteration-type-combobox
      (let [data-alteration-type-prop (jfx/map-entry-property dialog-state, (maybe-add-prefix [:alteration-type]))
            gui-alteration-type-prop (t/setup-alteration-type-combobox alteration-type-combobox,
                                       (-> dialog-state deref :gene-sample-data :alteration-type-set)
                                       :selected-alteration-type (jfx/value data-alteration-type-prop))]
        (jfx/bind data-alteration-type-prop gui-alteration-type-prop)))))


(defn setup-plot
  [data-mgmt, plot-ref, plot-parent, dialog-state, sort-settings-prefix]
  (let [[width, height] (jfx/layout-bounds plot-parent),
        maybe-add-prefix (if sort-settings-prefix (partial into (u/->vector sort-settings-prefix)) identity)
        gene-sample-data-prop (jfx/map-entry-property dialog-state, (maybe-add-prefix [:gene-sample-data]))
        sort-settings-prop (jfx/map-property dialog-state,
                             #{:gene-sample-data, :plot-style, :weighting, :overlap, :by-group?, :sample-group-first?,
                               :exclude-fixed-rows?, :ignore-fixed-rows-in-column-sort?, :show-preview?,
                               :only-sample-group, :alteration-type},
                             sort-settings-prefix),
        plot-style (let [current-state (deref dialog-state)
                         plot-style (get-in current-state (maybe-add-prefix [:plot-style]))
                         alteration-type (get-in current-state (maybe-add-prefix [:alteration-type]))]
                     (assoc plot-style :shown-alteration-type alteration-type))
        sorted-gene-sample-data (let [{:keys [show-preview?] :as sort-settings} (jfx/value sort-settings-prop),
                                      gene-sample-data (jfx/value gene-sample-data-prop)]
                                  (if show-preview?
                                    (ops/greedy-setcover-sorting gene-sample-data, sort-settings)
                                    (assoc gene-sample-data :gene-list []))),
        plot-ref (plot/create-plot sorted-gene-sample-data, plot-style,
                   width, height, {:plot-ref plot-ref,
                                   :show-header? false,
                                   :snapshot-handler (partial pexp/save-plot-as-snapshot data-mgmt),
                                   :show-objectives-handler (partial sv/show-objectives-view data-mgmt)}),]
    (jfx/add-child plot-parent, (plot/root-control plot-ref))
    ; resizing
    (jfx/listen-to
      (fn [^Bounds layout-bounds]
        (plot/resize-plot plot-ref, (.getWidth layout-bounds), (.getHeight layout-bounds)))
      (jfx/property ^Node plot-parent, :layout-bounds))
    ; sort settings modified
    (jfx/listen-to
      (fn [{:keys [gene-sample-data, plot-style, show-preview?, alteration-type] :as settings}]
        (if show-preview?
          (let [sorted-gene-sample-data (ops/greedy-setcover-sorting gene-sample-data, settings)]
            (plot/update-data! plot-ref sorted-gene-sample-data, (assoc plot-style :shown-alteration-type alteration-type)))
          (plot/update-data! plot-ref, (assoc gene-sample-data :gene-list []), plot-style)))
      sort-settings-prop)))


(defn show-dialog
  [ui, gene-sample-data, {:keys [shown-alteration-type] :as plot-style}, on-success-fn]
  (jfx/run-later
    (let [show-preview? (<= (-> gene-sample-data :gene-list count) 1000),
          dialog-state (atom {:weighting 0.5,
                              :overlap :minimize,
                              :only-sample-group nil,
                              :by-group? true,
                              :sample-group-first? true,
                              :exclude-fixed-rows? true,
                              :ignore-fixed-rows-in-column-sort? false,
                              :gene-sample-data gene-sample-data,
                              :alteration-type shown-alteration-type,
                              :plot-style plot-style,
                              :show-preview? show-preview?,
                              :dialog-result nil}),
          plot (ref nil),
          control (jfx/create-control "avatar/ui/GeneSortDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [plot-parent
                  apply-button
                  cancel-button] :as children} (jfx/control-children control)
          window (doto (jfx/modal-window "Sort Genes", control-node)
                   (t/add-avatar-icon))]
      ; bind sort settings
      (bind-sort-setting-controls children, dialog-state, nil)
      ; setup plot
      (jfx/handle-event! window, :window-shown,
        (fn [_]
          (setup-plot (:data-management ui), plot, plot-parent, dialog-state, nil)))

      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (let [{:keys [show-preview?, gene-sample-data, alteration-type] :as sort-settings} (deref dialog-state),
                result-data (if show-preview?
                              (plot/data plot)
                              (ops/greedy-setcover-sorting gene-sample-data, sort-settings))]
            (jfx/close window)
            (on-success-fn result-data, alteration-type))))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      #_(add-watch dialog-state :debug
          (fn [_, _, old-value, new-value]
            (u/print-modifications "settings" old-value, new-value)))

      (jfx/show window))))