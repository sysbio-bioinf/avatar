; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.settings
  (:require [avatar.data.management :as dm]
            [clj-jfx.core :as jfx]
            [clj-jfx.font-selection :as font-sel]
            [clj-jfx.colors :as col]
            [avatar.ui.plot.colors :as colors]
            [avatar.util :as u]
            [clj-jfx.table :as table]
            [avatar.ui.tools :as t])
  (:import (javafx.scene.control Button TextField ColorPicker CheckBox)))



(def plot-fonts
  #{:gene-group-font, :sample-group-font, :alteration-ratio-font, :gene-name-left-font, :gene-name-right-font})


(defn plot-style-fonts->maps
  [plot-style-map]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k
          (cond-> v (contains? plot-fonts k) jfx/font-map)))
      (transient {})
      plot-style-map)))


(defn ensure-plot-style-fonts
  [plot-style-map]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k
          (cond-> v (contains? plot-fonts k) jfx/font)))
      (transient {})
      plot-style-map)))


(defn set-button-font
  [plot-style, dialog, button-id, font-property]
  (let [button (jfx/find-node dialog, button-id)]
    (jfx/text-font! button, (get plot-style font-property))))


(def settings-font-button-map
  {"gene-group-font-button" :gene-group-font,
   "sample-group-font-button" :sample-group-font,
   "alteration-ratio-font-button" :alteration-ratio-font,
   "gene-name-left-font-button" :gene-name-left-font,
   "gene-name-right-font-button" :gene-name-right-font,})



(defn sync-settings-values
  [settings-dialog, plot-style-atom, bind?]
  (let [update-gene-name-example (fn []
                                   (let [plot-style (deref plot-style-atom),
                                         gene-name-example-textflow (jfx/find-node settings-dialog, "gene-name-example-textflow")]
                                     (jfx/textflow-texts! gene-name-example-textflow
                                       [{:text "FLT3" :font (:gene-name-left-font plot-style)}
                                        {:text (:gene-name-separator plot-style)}
                                        {:text "TKD" :font (:gene-name-right-font plot-style)}])))
        plot-style (deref plot-style-atom)]
    ; set current colors
    (doseq [attribute [:missing-value-color
                       :even-row-color
                       :alteration-color
                       :overlap-color
                       :selection-color
                       :odd-column-group-color
                       :odd-row-color
                       :border-color
                       :fixed-row-header-color
                       :even-column-group-color]]
      (let [color-picker-id (str (name attribute) "-picker"),
            ^ColorPicker
            color-picker (jfx/find-node settings-dialog, color-picker-id)]
        (jfx/selected-color! color-picker (get plot-style attribute))
        (when bind?
          (jfx/bind-to-atom (.valueProperty color-picker)
            (fn [_, new-value]
              (swap! plot-style-atom assoc attribute (col/color->str new-value)))))))


    (let [^TextField gene-name-separator-textfield (-> (jfx/find-node settings-dialog, "gene-name-separator-textfield")
                                                     (jfx/text-field-text! (:gene-name-separator plot-style)))]
      (when bind?
        (jfx/bind-to-atom (.textProperty gene-name-separator-textfield),
          (fn [_, new-value]
            (swap! plot-style-atom assoc :gene-name-separator new-value)
            (update-gene-name-example)))))
    (doseq [[button-id, font-property] settings-font-button-map]
      (set-button-font plot-style, settings-dialog, button-id, font-property))
    (update-gene-name-example)))


(defn on-font-button-click
  [plot-style-atom, font-attribute]
  (when-let [selected-font (font-sel/select-font {:initial-font (get @plot-style-atom font-attribute)})]
    (swap! plot-style-atom assoc font-attribute selected-font)))


(defn keyword+postfix
  [kw, postfix]
  (keyword (str (name kw) (name postfix))))


(defn bind-colors
  [plot-style-atom, children]
  (doseq [attribute [:missing-value-color
                     :even-row-color
                     :alteration-color
                     :overlap-color
                     :selection-color
                     :odd-row-color
                     :border-color
                     :fixed-row-header-color]]
    (let [color-picker-id (keyword+postfix attribute, "-picker"),
          ^ColorPicker color-picker (get children color-picker-id)]
      (jfx/bind-bidirectional (jfx/property color-picker, :value)
        (jfx/map-entry-property plot-style-atom, [attribute]))))
  (let [{:keys [^CheckBox show-missing-value-checkbox,
                ^ColorPicker missing-value-color-picker]} children]
    (jfx/bind-bidirectional (jfx/property show-missing-value-checkbox, :selected)
      (jfx/map-entry-property plot-style-atom, [:paint-missing-values?]))
    (jfx/bind (jfx/property missing-value-color-picker, :disable),
      (jfx/functional-property not (jfx/property show-missing-value-checkbox, :selected)))))


(defn setup-textfield
  [plot-style-atom, property-kw, ^TextField textfield, format]
  (jfx/bind-bidirectional (jfx/property textfield, :text),
    (jfx/map-entry-property plot-style-atom, [property-kw]))
  (jfx/text-field-format! textfield, format, #(<= 0 %))
  textfield)


(defn setup-font-buttons
  [plot-style-atom, children]
  (doseq [attribute [:gene-group-font, :sample-group-font, :alteration-ratio-font, :gene-name-left-font, :gene-name-right-font]]
    (let [button-id (keyword+postfix attribute, "-button"),
          ^Button button (get children button-id)]
      (jfx/bind-bidirectional (jfx/property button, :font),
        (jfx/map-entry-property plot-style-atom [attribute]))
      (jfx/handle-event! button,
        :action
        (fn [_] (on-font-button-click plot-style-atom, attribute))))))


(defn numbers->strings
  [plot-style]
  (-> plot-style
    (update-in [:border-width] u/format-double)
    (update-in [:alteration-cell-width] str)
    (update-in [:alteration-ratio-digits] str)))


(defn strings->numbers
  [plot-style]
  (-> plot-style
    (update-in [:border-width] u/parse-double)
    (update-in [:alteration-cell-width] #(Long/parseLong %))
    (update-in [:alteration-ratio-digits] #(Long/parseLong %))))


(defn difference-map
  [old-rows, new-rows]
  (let [old-map (u/zipmap-by :group, :color, old-rows)]
    (persistent!
      (reduce
        (fn [diff-map, {:keys [group, color, update-path] :as row}]
          (let [old-color (get old-map group)]
            (cond-> diff-map
              (not= old-color color) (assoc! update-path color))))
        (transient {})
        new-rows))))

(defn setup-sample-group-color-controls
  [plot-style-atom, children]
  (let [{:keys [even-odd-coloring-radiobutton,
                custom-colors-radiobutton,
                sample-group-colors-tableview]} children
        toggle-group (jfx/toggle-group even-odd-coloring-radiobutton, custom-colors-radiobutton),
        toggle-sample-groups-colored? (fn [sample-groups-colored?]
                                        (.selectToggle toggle-group (if sample-groups-colored? custom-colors-radiobutton even-odd-coloring-radiobutton)))
        sample-groups-colored?-prop (jfx/map-entry-property plot-style-atom, [:sample-groups-colored?])
        table-rows-atom (atom [])]
    ; setup toggle group for :sample-groups-colored?
    (toggle-sample-groups-colored? (jfx/value sample-groups-colored?-prop))
    ; update radiobutton state on property change
    (jfx/listen-to
      toggle-sample-groups-colored?
      sample-groups-colored?-prop)
    ; change property when different toggle has been selected
    (jfx/listen-to
      (fn [selected-toggle]
        (jfx/value! sample-groups-colored?-prop (= selected-toggle custom-colors-radiobutton)))
      (jfx/property toggle-group, :selected-toggle))
    ; update plot style based on changes of the rows
    (add-watch table-rows-atom :update-plot-style
      (fn [_, _, old-rows, new-rows]
        ; ignore switching of color modes
        (when (= (count old-rows) (count new-rows))
          (let [diff-map (difference-map old-rows, new-rows)]
            (when (seq diff-map)
              (swap! plot-style-atom
                #(reduce-kv
                   (fn [plot-style, update-path, color]
                     (assoc-in plot-style update-path color))
                   %
                   diff-map)))))))

    (jfx/setup-table-view! sample-group-colors-tableview, [:group, :color] table-rows-atom,
      :column-cell-factories {:color (fn [] (table/color-chooser-cell))})

    ; choose rows based on coloring mode
    (jfx/invalidate!
      (jfx/listen-to
        (fn [sample-groups-colored?, {:keys [even-column-group-color, odd-column-group-color]}, sample-group->color]
          (if sample-groups-colored?
            (reset! table-rows-atom
            (->> sample-group->color
              (sort-by first)
              (mapv
                (fn [[sample-group, color]]
                    {:group sample-group, :color color, :update-path [:sample-group->color, sample-group]}))))
            (reset! table-rows-atom
            [{:group "even column group", :color even-column-group-color, :update-path [:even-column-group-color]}
               {:group "odd column group", :color odd-column-group-color, :update-path [:odd-column-group-color]}])))
        sample-groups-colored?-prop
        (jfx/map-property plot-style-atom, [:even-column-group-color, :odd-column-group-color])
        (jfx/map-entry-property plot-style-atom, [:sample-group->color])))))


(defn show-dialog
  [ui]
  (jfx/run-now
    (let [data-mgmt (:data-management ui),
          plot-style-atom (atom
                            (-> (dm/plot-style data-mgmt)
                              colors/ensure-plot-style-colors
                              ensure-plot-style-fonts
                              numbers->strings)),
          control (jfx/create-control "avatar/ui/SettingsDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [^TextField border-width-textfield,
                  ^TextField alteration-cell-width-textfield,
                  ^TextField alteration-ratio-digits-textfield,
                  ^TextField gene-name-separator-textfield
                  gene-name-example-textflow,
                  apply-button
                  cancel-button
                  default-button]
           :as children} (jfx/control-children control),
          window (-> (jfx/modal-window "Settings", control-node)
                   (jfx/resizable! false)
                   (t/add-avatar-icon))]
      ; bind colors in static color pickers (not in sample group table)
      (bind-colors plot-style-atom, children)
      ; bind textfields with numbers
      (setup-textfield plot-style-atom, :border-width, border-width-textfield, (jfx/decimal-format))
      (setup-textfield plot-style-atom, :alteration-cell-width, alteration-cell-width-textfield, (jfx/integer-format))
      (setup-textfield plot-style-atom, :alteration-ratio-digits, alteration-ratio-digits-textfield, (jfx/integer-format))
      ; bind gene example
      (jfx/bind-bidirectional (jfx/property gene-name-separator-textfield, :text)
        (jfx/map-entry-property plot-style-atom, [:gene-name-separator]))
      ; listen to properties for gene textflow example and invalidate immediately to show initial fonts
      (jfx/invalidate!
        (jfx/listen-to
          (fn [{:keys [gene-name-left-font, gene-name-separator, gene-name-right-font]}]
            (jfx/textflow-texts! gene-name-example-textflow
              [{:text "FLT3" :font gene-name-left-font}
               {:text gene-name-separator}
               {:text "TKD" :font gene-name-right-font}]))
          (jfx/map-property plot-style-atom, [:gene-name-left-font, :gene-name-separator, :gene-name-right-font])))
      ; bind font buttons
      (setup-font-buttons plot-style-atom, children)
      ; setup sample group colors controls
      (setup-sample-group-color-controls plot-style-atom, children)
      ; button event handling
      (jfx/handle-event! apply-button, :action
        (fn [_]
          (let [plot-style (-> plot-style-atom
                             deref
                             colors/plot-style-colors->strs
                             plot-style-fonts->maps
                             strings->numbers)]
            (dm/plot-style! data-mgmt, plot-style))
          (jfx/close window)))
      (jfx/handle-event! cancel-button, :action
        (fn [_]
          (jfx/close window)))
      (jfx/handle-event! default-button, :action
        (fn [_]
          (reset! plot-style-atom
            (let [{:keys [sample-group->color]} (deref plot-style-atom)
                  default-plot-style (assoc (dm/default-plot-style)
                                       :sample-group->color
                                       (zipmap (sort (keys sample-group->color)) (cycle dm/kelly-colors)))]
              (-> default-plot-style
                colors/ensure-plot-style-colors
                ensure-plot-style-fonts
                numbers->strings)))))
      (jfx/show-and-wait window))))
