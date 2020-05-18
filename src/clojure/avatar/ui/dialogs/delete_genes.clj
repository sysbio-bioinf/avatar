; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.delete-genes
  (:require [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [avatar.ui.tools :as t]
            [avatar.data.gene-data :as gd])
  (:import
    (javafx.scene.text Text)
    (javafx.scene.control Button Slider RadioButton)))



(defn selected-gene-count
  [gene-sample-data, minimal-alterations, maximal-alterations, alteration-type-selection]
  (count
    (gd/find-genes-in-alteration-count-range
      alteration-type-selection,
      minimal-alterations,
      maximal-alterations,
      gene-sample-data)))


(defn setup-slider
  [^Slider slider, ^Text text, selection-state, value-key, min-alterations, max-alterations, sample-count, display-mode-prop]
  (jfx/listen-to
    (fn [display-mode]
      (let [{:keys [min-value, max-value]} (case display-mode
                                             :absolute {:min-value min-alterations,
                                                        :max-value max-alterations}
                                             :relative {:min-value 0.0, :max-value 1.0})
            value (case value-key
                    :minimal-alterations min-value
                    :maximal-alterations max-value)]
        (doto slider
          (jfx/property-value! :min, min-value)
          (jfx/property-value! :max, max-value)
          (jfx/property-value! :value, value))))
    display-mode-prop)

  (jfx/listen-to
    (fn [min-value, max-value, display-mode]
      (let [tick-length (case display-mode
                          :absolute (max (long (/ (- max-value min-value) 10.0)) 1)
                          :relative (max (u/round-to 2 (/ (- max-value min-value) 10.0)) 0.01)),
            increment (case display-mode
                        :absolute 1
                        :relative 0.01)]
        (doto slider
          (jfx/property-value! :major-tick-unit, tick-length)
          (jfx/property-value! :block-increment, increment))))
    (jfx/property slider, :min),
    (jfx/property slider, :max),
    display-mode-prop)


  (let [value-prop (jfx/map-entry-property selection-state, [value-key]),
        slider-value-prop (jfx/property slider, :value)]

    ; bind property to slide value, round to long and convert to relative value if needed
    (jfx/bind value-prop,
      (jfx/functional-property
        (fn [slider-value, display-mode]
          (case display-mode
            :absolute (long slider-value)
            :relative (long (* slider-value sample-count))))
        slider-value-prop
        display-mode-prop))

    ; listen to value changes and update slider value
    (jfx/listen-to
      (fn [value, display-mode]
        (let [slider-value (case display-mode
                             :absolute value
                             :relative (u/round-to 2, (/ value (double sample-count))))]
          (jfx/property-value! slider, :value slider-value)))
      value-prop,
      display-mode-prop)

    (jfx/bind (jfx/property text, :text)
      (jfx/functional-property
        (fn [value, display-mode]
          (when value
            (case display-mode
              :absolute (-> value long str)
              :relative (->> (/ value (double sample-count)) (u/round-to 2) (* 100) long (format "%d%%")))))
        value-prop
        display-mode-prop))))


(defn show-dialog
  [{:keys [alteration-type-set, gene-list, sample-count] :as gene-sample-data}, selected-alteration-type, display-mode]
  (jfx/run-now
    (let [min-alterations 0,
          max-alterations sample-count,
          display-mode (or display-mode :relative),
          ; init with 0, on :window-shown set min-alterations and max-alterations to draw colors correctly
          dialog-state (atom {:minimal-alterations 0, :maximal-alterations 0,
                              ; different display mode to trigger update on window shown
                              :display-mode (case display-mode
                                              :absolute :relative
                                              :relative :absolute)})
          result (atom nil),
          control (jfx/create-control "avatar/ui/SelectGenesByAlterationCountDialog.fxml")
          control-node (jfx/control-node control),
          {:keys [checkbox-container,
                  ^Slider minimal-alterations-slider,
                  ^Slider maximal-alterations-slider,
                  minimal-alterations-text,
                  maximal-alterations-text,
                  ^RadioButton
                  display-relative-radiobutton,
                  ^RadioButton
                  display-absolute-radiobutton,
                  ^Text selected-gene-count-text,
                  ^Button import-button,
                  cancel-button]} (jfx/control-children control),
          minimal-alterations-prop (jfx/map-entry-property dialog-state, :minimal-alterations),
          maximal-alterations-prop (jfx/map-entry-property dialog-state, :maximal-alterations),
          display-mode-prop (jfx/map-entry-property dialog-state, :display-mode),
          display-mode-togglegroup (.getToggleGroup display-relative-radiobutton),
          {:keys [checkbox-control, selection-state]} (t/setup-alteration-type-checkbox-control alteration-type-set, selected-alteration-type),
          window (doto (jfx/modal-window "Delete genes", control-node)
                   (t/add-avatar-icon))]

      (jfx/add-child checkbox-container checkbox-control)

      (case display-mode
        :absolute (.setSelected display-absolute-radiobutton true)
        :relative (.setSelected display-relative-radiobutton true))

      (jfx/listen-to
        (fn [selected-toggle]
          (let [display-mode (cond
                               (= selected-toggle display-relative-radiobutton) :relative
                               (= selected-toggle display-absolute-radiobutton) :absolute)]
            (swap! dialog-state assoc :display-mode display-mode)))
        (jfx/property display-mode-togglegroup, :selected-toggle))

      (setup-slider minimal-alterations-slider, minimal-alterations-text, dialog-state, :minimal-alterations, min-alterations, max-alterations, sample-count, display-mode-prop)
      (setup-slider maximal-alterations-slider, maximal-alterations-text, dialog-state, :maximal-alterations, min-alterations, max-alterations, sample-count, display-mode-prop)

      (jfx/listen-to
        (fn [min-value]
          (jfx/property-value! maximal-alterations-slider, :min, (u/round-to 2, min-value)))
        (jfx/property minimal-alterations-slider, :value))

      (jfx/listen-to
        (fn [max-value]
          (jfx/property-value! minimal-alterations-slider, :max, (u/round-to 2, max-value)))
        (jfx/property maximal-alterations-slider, :value))

      (jfx/bind (jfx/property selected-gene-count-text, :text) (jfx/functional-property
                                                                 (comp str (partial selected-gene-count gene-sample-data))
                                                                 minimal-alterations-prop
                                                                 maximal-alterations-prop
                                                                 (jfx/->property selection-state)))

      (jfx/handle-event! window :window-shown
        (fn [_]
          (swap! dialog-state assoc :minimal-alterations min-alterations, :maximal-alterations max-alterations, :display-mode display-mode)))

      (jfx/handle-event! import-button, :action,
        (fn [_]
          (let [{:keys [minimal-alterations, maximal-alterations]} (deref dialog-state)]
            (reset! result {:minimal-alterations (long minimal-alterations),
                            :maximal-alterations (long maximal-alterations)
                            :alteration-type-set (deref selection-state)}))
          (jfx/close window)))
      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))
      (jfx/show-and-wait window)
      (deref result))))