; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.statistics-view
  (:require [clj-jfx.core :as jfx]
            [avatar.ui.plot :as plot]
            [avatar.ui.plot-export :as pexp]
            [avatar.data.operations :as ops]
            [avatar.util :as u]
            [clojure.string :as str]
            [clj-jfx.selection :as jfx-select]
            [avatar.algorithms.common :as c]
            [avatar.ui.tools :as t]
            [clj-jfx.properties :as props]
            [avatar.data.gene-data :as gd])
  (:import (javafx.scene Node)
           (javafx.geometry Bounds)))



(defn create-plot
  [data-mgmt, parent-control, plot, gene-sample-data, plot-style]
  (let [[width, height] (jfx/layout-bounds parent-control),
        plot (plot/create-plot gene-sample-data, plot-style, width, height,
               {:plot-ref plot, :show-header? false,
                :snapshot-handler (partial pexp/save-plot-as-snapshot data-mgmt)})]
    (jfx/add-child parent-control, (plot/root-control plot))
    ; resizing
    (jfx/listen-to
      (fn [^Bounds layout-bounds]
        (plot/resize-plot plot, (.getWidth layout-bounds), (.getHeight layout-bounds)))
      (jfx/property ^Node parent-control, :layout-bounds))))


(defn objective-table-rows
  [value-mode, selected-objective, gene-groups, sample-groups, objective-per-groups]
  (let [value-fmt (case value-mode
                    :relative "%.3f"
                    :absolute "%d")]
    (mapv
      (fn [gene-group]
        (reduce
          (fn [row-map, sample-group]
            (assoc row-map
              sample-group
              (format value-fmt
                (get-in objective-per-groups [[gene-group, sample-group], value-mode, selected-objective]))))
          {:gene-group gene-group}
          sample-groups))
      gene-groups)))


(defn update-objectives-table
  [gene-sample-data, objectives-table, value-mode, selected-objective, objective-calculation-mode, alteration-type]
  (let [{:keys [sample-groups, gene-groups, objective-per-groups]} (->> (cond-> gene-sample-data
                                                                          (= objective-calculation-mode :without-fixed-genes)
                                                                          (update-in [:gene-list] (partial u/removev :order-fixed?)))
                                                                     (c/select-alteration-data alteration-type)
                                                                     ops/objective-per-groups),
        column-specs (list* {:text "Groups", :style "-fx-alignment: CENTER-LEFT;"}
                       (mapv
                         (fn [group] {:text group, :style "-fx-alignment: CENTER-RIGHT;"})
                         sample-groups))
        table-rows (objective-table-rows value-mode, selected-objective, gene-groups, sample-groups, objective-per-groups)],
    (jfx/table-columns! objectives-table, column-specs)
    (jfx/setup-table-view! objectives-table,
      (list* :gene-group, sample-groups),
      (atom table-rows),
      :sort? true)
    (jfx-select/enable-multiple-selection! objectives-table)
    (jfx/enable-copy-to-clipboard! objectives-table)
    {:table-rows table-rows,
     :columns (mapv :text column-specs),
     :column-keys (list* :gene-group, sample-groups)}))


(defn alterations-table-rows
  [value-mode, gene-names, sample-groups, grouped-alteration-data]
  (let [value-fmt (case value-mode
                    :relative "%.3f"
                    :absolute "%d")]
    (mapv
      (fn [gene-name]
        (reduce
          (fn [row-map, sample-group]
            (assoc row-map
              sample-group
              (format value-fmt
                (get-in grouped-alteration-data [[gene-name, sample-group], value-mode]))))
          {:gene-name gene-name}
          sample-groups))
      gene-names)))


(defn update-alterations-table
  [gene-sample-data, alterations-table, value-mode, alteration-type]
  (let [{:keys [sample-groups, gene-names, grouped-alteration-data]} (->> gene-sample-data
                                                                       (c/select-alteration-data alteration-type)
                                                                       (ops/alterations-per-sample-group-and-gene)),
        column-specs (list* {:text "Genes", :style "-fx-alignment: CENTER-LEFT;"}
                       (mapv
                         (fn [group] {:text group, :style "-fx-alignment: CENTER-RIGHT;"})
                         sample-groups)),
        table-rows (alterations-table-rows value-mode, gene-names, sample-groups, grouped-alteration-data)]
    (jfx/table-columns! alterations-table, column-specs)
    (jfx/setup-table-view! alterations-table,
      (list* :gene-name, sample-groups),
      (atom table-rows)
      :sort? true)
    (jfx-select/enable-multiple-selection! alterations-table)
    (jfx/enable-copy-to-clipboard! alterations-table)
    {:table-rows table-rows,
     :columns (mapv :text column-specs),
     :column-keys (list* :gene-name, sample-groups)}))


(defn clinical-attribute-table-rows
  [value-mode, clinical-attribute-values, sample-groups, grouped-clinical-attribute-data]
  (let [value-fmt (case value-mode
                    :relative "%.3f"
                    :absolute "%d")
        default-value (case value-mode
                        :relative 0.0
                        :absolute 0)]
    (mapv
      (fn [attribute-value]
        (reduce
          (fn [row-map, sample-group]
            (assoc row-map
              sample-group
              (format value-fmt
                (or
                  (get-in grouped-clinical-attribute-data [sample-group, value-mode, attribute-value])
                  default-value))))
          {:attribute-value attribute-value}
          sample-groups))
      clinical-attribute-values)))


(defn update-clinical-attribute-table
  [gene-sample-data, clinical-attribute-table, value-mode, selected-clinical-attribute]
  (let [{:keys [sample-groups, clinical-attribute-values, grouped-clinical-attribute-data]} (ops/clinical-attribute-values-frequencies-per-sample-group gene-sample-data, selected-clinical-attribute),
        column-specs (list* {:text "Values", :style "-fx-alignment: CENTER-LEFT;"}
                       (mapv
                         (fn [group] {:text group, :style "-fx-alignment: CENTER-RIGHT;"})
                         sample-groups)),
        table-rows (clinical-attribute-table-rows value-mode, clinical-attribute-values, sample-groups, grouped-clinical-attribute-data)]
    (jfx/table-columns! clinical-attribute-table, column-specs)
    (jfx/setup-table-view! clinical-attribute-table,
      (list* :attribute-value, sample-groups),
      (atom table-rows)
      :sort? true)
    (jfx-select/enable-multiple-selection! clinical-attribute-table)
    (jfx/enable-copy-to-clipboard! clinical-attribute-table)
    {:table-rows table-rows,
     :columns (mapv :text column-specs),
     :column-keys (list* :attribute-value, sample-groups)}))


(defn export-table-data
  [file, {:keys [table-rows, column-keys, columns], :as table-data}]
  (let [sep (u/column-separator file)]
    (spit file,
      (->> table-rows
        (mapv
          (fn [row-map]
            (let [row-vec (mapv #(get row-map %) column-keys)]
              (str/join
                sep
                (update-in row-vec [0] u/quoted-str)))))
        (list* (->> columns (mapv u/quoted-str) (str/join sep)))
        (str/join "\n")))))


(def alteration-type-order-map (zipmap [:mutation-data, :expression-data, :methylation-data] (range)))

(defn show-objectives-view
  [data-mgmt, {:keys [clinical-attributes, alteration-type-set] :as gene-sample-data}, {:keys [shown-alteration-type] :as plot-style}]
  (jfx/run-now
    (let [dialog-state (atom {}),
          control (jfx/create-control "avatar/ui/PlotStatisticsView.fxml"),
          control-node (jfx/control-node control),
          {:keys [plot-parent,
                  relative-values-radiobutton,
                  absolute-values-radiobutton,
                  without-fixed-genes-radiobutton,
                  all-genes-radiobutton,
                  statistics-tabpane,
                  objectives-tab,
                  alterations-tab,
                  clinical-attributes-tab,
                  objective-combobox,
                  clinical-attribute-combobox,
                  alteration-type-combobox,
                  objectives-table,
                  alterations-table,
                  clinical-attribute-table,
                  export-table-data-menu,
                  export-plot-data-menu,
                  gene-count-label,
                  sample-count-label,
                  close-button] :as children} (jfx/control-children control),
          window (-> (jfx/window "Plot with statistics", control-node)
                   ; prevent atom from being garbage collected
                   (jfx/user-data! dialog-state)
                   (t/add-avatar-icon))
          plot (ref nil),
          value-mode-prop (jfx/map-entry-property dialog-state, [:value-mode]),
          objective-calculation-mode-prop (jfx/map-entry-property dialog-state, [:objective-calculation-mode])
          selected-statistics-prop (jfx/map-entry-property dialog-state, [:selected-statistics])
          selected-objective-prop (jfx/map-entry-property dialog-state, [:selected-objective])
          selected-clinical-attribute-prop (jfx/map-entry-property dialog-state, [:selected-clinical-attribute])
          selected-alteration-type-prop (t/setup-alteration-type-combobox alteration-type-combobox, alteration-type-set,
                                          :selected-alteration-type shown-alteration-type)]

      ; gene count
      (props/set-property gene-count-label :text (str (gd/gene-count gene-sample-data)))
      ; sample count
      (props/set-property sample-count-label :text (str (gd/sample-count gene-sample-data)))

      ; objectives table update handler
      (jfx/listen-to
        (fn [selected-statistics, value-mode, selected-objective, objective-calculation-mode, selected-alteration-type, selected-clinical-attribute]
          (when selected-statistics
            (when-let [table-data (case selected-statistics
                                    :objectives (update-objectives-table gene-sample-data, objectives-table, value-mode, selected-objective, objective-calculation-mode, selected-alteration-type)
                                    :alterations (update-alterations-table gene-sample-data, alterations-table, value-mode, selected-alteration-type)
                                    :clinical-attributes (update-clinical-attribute-table gene-sample-data, clinical-attribute-table, value-mode, selected-clinical-attribute))]
              (swap! dialog-state assoc :current-table-data table-data))))
        selected-statistics-prop,
        value-mode-prop
        selected-objective-prop,
        objective-calculation-mode-prop,
        selected-alteration-type-prop,
        selected-clinical-attribute-prop)

      ; show relative or absolute values?
      (jfx/bind value-mode-prop
        (jfx/selected-toggle-property
          {:relative relative-values-radiobutton,
           :absolute absolute-values-radiobutton}))

      ; calculate objective with or without fixed genes?
      (jfx/bind objective-calculation-mode-prop
        (jfx/selected-toggle-property
          {:without-fixed-genes without-fixed-genes-radiobutton,
           :all-genes all-genes-radiobutton}))

      ; selected tab
      (jfx/bind selected-statistics-prop
        (jfx/value->keyword-property
          {:objectives objectives-tab,
           :alterations alterations-tab,
           :clinical-attributes clinical-attributes-tab}
          (jfx-select/selected-item-property statistics-tabpane)))

      ; update plot
      (jfx/listen-to
        (fn [selected-alteration-type]
          (plot/shown-alteration-type! plot, selected-alteration-type))
        selected-alteration-type-prop)

      ; selected objective
      (jfx/setup-combobox-first-selected! objective-combobox, ["coverage", "overlap"])
      (jfx/bind selected-objective-prop
        (jfx/value->keyword-property
          {:coverage "coverage",
           :overlap "overlap"}
          (jfx-select/selected-item-property objective-combobox)))

      ; selected clinical attribute
      (jfx/setup-combobox-first-selected! clinical-attribute-combobox,
        (sort-by str/upper-case (disj clinical-attributes "SAMPLE_ID", "PATIENT_ID", "OTHER_SAMPLE_ID", "OTHER_PATIENT_ID")))
      (jfx/bind selected-clinical-attribute-prop
        (jfx-select/selected-item-property clinical-attribute-combobox))

      ; export table data menu item
      (jfx/set-menu-item-action! export-table-data-menu,
        (fn [_]
          (when-let [file (t/ask-for-text-export-file window, "table")]
            (export-table-data file, (:current-table-data (deref dialog-state))))))

      ; export plot data menu item
      (jfx/set-menu-item-action! export-plot-data-menu,
        (fn [_]
          (when-let [file (t/ask-for-text-export-file window, "plot")]
            (plot/export-plot-data file, plot))))

      (jfx/handle-event! window, :window-shown
        (fn [_]
          (create-plot data-mgmt, plot-parent, plot, gene-sample-data,
            (assoc plot-style :shown-alteration-type (jfx/value selected-alteration-type-prop)))))

      (jfx/handle-event! close-button, :action, (fn [_] (jfx/close window)))

      (jfx/show window))))
