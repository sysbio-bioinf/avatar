; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot.structure
  (:require
    [avatar.ui.plot.data :as pdata]
    [avatar.ui.plot.layout :as lo]
    [avatar.util :as u]
    [clj-jfx.core :as jfx]
    [clj-jfx.colors :as col]
    [clj-jfx.properties :as props])
  (:import
    (javafx.scene.control ToggleButton Label ScrollBar MenuButton MenuItem SeparatorMenuItem)
    (javafx.scene.layout GridPane VBox)
    (javafx.geometry Orientation Insets)
    (avatar.ui.plot.type ITextCell)
    (javafx.scene.text TextFlow Text)
    (javafx.scene.canvas Canvas)
    (de.jensd.fx.glyphs.materialdesignicons MaterialDesignIconView MaterialDesignIcon)))



(defn create-row-header
  [row-index]
  (doto (ToggleButton. (str (inc row-index)))
    (jfx/max-size! Double/MAX_VALUE, Double/MAX_VALUE)
    jfx/grow-always-in-grid!
    (jfx/alignment! :baseline-center)))


(defn growing-grid-pane
  []
  (jfx/grow-always-in-grid! (GridPane.)))


(defn create-column-header
  [column-width, column-index]
  (doto (ToggleButton. (str (inc column-index)))
    (jfx/min-width! column-width)
    (jfx/max-width! column-width)
    (jfx/max-height! Double/MAX_VALUE)
    jfx/grow-always-in-grid!
    (jfx/alignment! :baseline-center)))


(defn create-header-label
  [row-height, caption]
  (doto (Label. (str caption))
    (jfx/min-height! row-height)
    (jfx/max-height! Double/MAX_VALUE)
    (jfx/max-width! Double/MAX_VALUE)
    ; borrow button style
    (.setStyle
      (str
        "-fx-background-color: -fx-shadow-highlight-color, -fx-outer-border, -fx-inner-border, -fx-body-color ;"
        "-fx-background-insets: 0 0 -1 0, 0, 1, 2            ;"
        "-fx-background-radius: 3px, 3px, 2px, 1px           ;"
        "-fx-text-fill: -fx-text-base-color                  ;"
        "-fx-alignment: CENTER                               ;"
        "-fx-content-display: LEFT                           ;"))))


(defn create-scrollbars
  []
  (let [vertical-scrollbar (doto (ScrollBar.)
                             (.setOrientation Orientation/VERTICAL)
                             (jfx/max-width! Double/MAX_VALUE)
                             jfx/grow-always-in-grid!
                             (jfx/property-value! :unit-increment 1.0)
                             (jfx/property-value! :block-increment 10.0)
                             (jfx/property-value! :min 0.0)
                             (jfx/property-value! :value 0.0)),
        horizontal-scrollbar (doto (ScrollBar.)
                               (.setOrientation Orientation/HORIZONTAL)
                               (jfx/max-height! Double/MAX_VALUE)
                               jfx/grow-always-in-grid!
                               (jfx/property-value! :unit-increment 1.0)
                               (jfx/property-value! :block-increment 10.0)
                               (jfx/property-value! :min 0.0)
                               (jfx/property-value! :value 0.0))]
    {:vertical-scrollbar vertical-scrollbar,
     :horizontal-scrollbar horizontal-scrollbar}))


(defn textcell?
  [cell]
  (instance? ITextCell cell))


(defn size!
  [node, width, height]
  (cond-> node
    width (-> (jfx/min-width! width) (jfx/max-width! width))
    height (-> (jfx/min-height! height) (jfx/max-height! height))
    (and (nil? width) (nil? height)) (jfx/max-size! Double/MAX_VALUE, Double/MAX_VALUE)))


(defn create-text-cell
  [text, {:keys [spacing, alignment]}]
  (let [spacing (or spacing 2.0),
        textflow (doto (TextFlow.)
                   (-> .getChildren (.add (Text. text)))
                   (jfx/textflow-textalignment! (or alignment :right))),
        textcell (proxy [VBox, ITextCell] []
                   (getTextFlow [] textflow))]
    (doto textcell
      (-> .getChildren (.add textflow))
      (.setPadding (Insets. spacing))
      jfx/grow-always-in-grid!
      (jfx/alignment! :center))))


(defn textflow
  ^TextFlow [^ITextCell textcell]
  (.getTextFlow textcell))


(defn add-headers
  [controls-map, plot-layout, header-type, width-key, n]
  (let [{:keys [headers]} (get-in controls-map [:headers, header-type]),
        width (get plot-layout width-key),
        header-count (count headers),
        create-header-fn (case header-type
                           :row create-row-header
                           :column #(create-column-header width, %))
        new-headers (mapv create-header-fn (range header-count (+ header-count n)))]
    ; store new headers and corresponding constraints
    (update-in controls-map [:headers, header-type, :headers] into new-headers)))


(defn create-text-cell-rows
  [controls-map, n, create-text-cell-fn]
  (let [{:keys [rows]} controls-map,
        row-count (count rows),
        new-text-cells (mapv create-text-cell-fn (range row-count (+ row-count n)))]
    new-text-cells))


(defn add-rows
  [controls-map, show-header?, plot-layout, n]
  (let [controls-map (cond-> controls-map
                       show-header? (add-headers plot-layout, :row, :row-header-width, n))
        group-text-cells (create-text-cell-rows controls-map, n,
                           (fn [row-index]
                             (create-text-cell nil, {:alignment :left}))),
        gene-text-cells (create-text-cell-rows controls-map, n,
                          (fn [row-index]
                            (create-text-cell "Gene", {:alignment :right}))),
        alteration-ratio-text-cells (create-text-cell-rows controls-map, n,
                                      (fn [_]
                                        (create-text-cell "100%", {:alignment :right}))),
        new-rows (mapv
                   #(zipmap [:group-name-control, :gene-name-control, :alteration-ratio-control] %&)
                   group-text-cells
                   gene-text-cells
                   alteration-ratio-text-cells)]
    ; store new rows
    (update-in controls-map [:rows] into new-rows)))


(defn create-row-controls
  [{:keys [alteration-row-count] :as plot-layout}, show-header?]
  (let [; create controls for each row
        controls-map {:row-group-grid-pane (growing-grid-pane),
                      :row-gene-grid-pane (growing-grid-pane),
                      :row-alteration-ratio-grid-pane (growing-grid-pane),
                      :rows []
                      :headers {:row {:grid-pane (growing-grid-pane), :headers []}}}]
    (add-rows controls-map, show-header?, plot-layout, alteration-row-count)))


(defn add-column-group-text-cells
  [controls-map, n]
  (let [columns (get-in controls-map [:columns, :sample-group-columns])
        column-count (count columns),
        new-columns (mapv
                      (fn [column-index]
                        (create-text-cell nil, {:alignment :center}))
                      (range column-count (+ column-count n)))]
    ; store new columns
    (update-in controls-map [:columns, :sample-group-columns] into new-columns)))


(defn add-columns
  [controls-map, show-header?, plot-layout, n]
  (cond-> controls-map
    show-header? (add-headers plot-layout, :column, :alteration-cell-width, n)
    true (add-column-group-text-cells n)))


(defn create-column-controls
  [{:keys [alteration-column-count] :as plot-layout}, show-header?]
  (let [controls-map {:column-group-grid-pane (growing-grid-pane),
                      :columns {:group-name-column-control (create-text-cell nil, {}),
                                :gene-name-column-control (create-text-cell nil, {}),
                                :sample-group-columns [],
                                :alteration-ratio-column-control (create-text-cell nil, {})}
                      :headers {:column {:grid-pane (growing-grid-pane), :headers []}}}]
    (add-columns controls-map, show-header?, plot-layout, alteration-column-count)))


(defn initial-view
  [{:keys [sample-count, gene-list] :as plot-data},
   {:keys [alteration-row-count, alteration-column-count, alteration-row-height, alteration-cell-width] :as plot-layout}]
  {:gene-count (count gene-list),
   :visible-gene-count alteration-row-count
   :first-gene-row-index 0
   :sample-count sample-count
   :visible-sample-count alteration-column-count
   :first-sample-column-index 0
   :alteration-cell-width alteration-cell-width
   :alteration-cell-height alteration-row-height})


(defn create-canvas
  [{:keys [alteration-canvas-width, alteration-canvas-height] :as plot-layout}]
  (jfx/grow-always-in-grid! (Canvas. alteration-canvas-width, alteration-canvas-height)))


(defn create-menu-item
  [^MaterialDesignIcon image, ^String text]
  (MenuItem. text, (doto (MaterialDesignIconView. image)
                     (.setSize "20"))))


(defn create-plot-controls
  [plot-layout, show-header?]
  (u/deep-merge
    (create-row-controls plot-layout, show-header?)
    (create-column-controls plot-layout, show-header?)
    {:canvas (create-canvas plot-layout)
     :plot-menu (doto (MenuButton. "Plot Actions")
                  (jfx/text-font! {:size 11})
                  (jfx/alignment! :center)
                  (jfx/min-height! 0)
                  (jfx/max-size! Double/MAX_VALUE, Double/MAX_VALUE))
     :export-button (create-menu-item MaterialDesignIcon/FILE_IMAGE, "Export plot graphic")
     :snapshot-button (create-menu-item MaterialDesignIcon/CAMERA, "Save plot as snapshot")
     :statistics-button (create-menu-item MaterialDesignIcon/TABLE_LARGE, "Open plot statistics view")
     :export-alteration-count-data-button (create-menu-item MaterialDesignIcon/NUMERIC, "Export alteration contingency tables")
     :export-alteration-data-button (create-menu-item MaterialDesignIcon/DATABASE, "Export alteration data")}))


(defn initialize
  [plot-map, ^long width, ^long height]
  (let [{:keys [state, show-header?]} plot-map,
        {:keys [plot-data, plot-style]} state,
        plot-layout (lo/calculate-plot-layout plot-data, plot-style, show-header?, width, height)]
    (let [controls-map (u/timing "build-grid-pane"
                         (u/deep-merge
                           (create-plot-controls plot-layout, show-header?)
                           (create-scrollbars)))]
      (-> plot-map
        (update-in [:controls] u/deep-merge controls-map)
        (update-in [:state] assoc
          :plot-layout plot-layout,
          :view (initial-view plot-data, plot-layout))))))


(defn remove-headers
  [controls-map, header-type, n]
  (let [{:keys [headers]} (get-in controls-map [:headers, header-type])
        header-count (count headers)]
    (cond-> controls-map
      (pos? header-count)
      ; remove headers
      (update-in [:headers, header-type, :headers] #(u/iteration n, pop, %)))))


(defn remove-text-cell-rows
  [controls-map, n]
  (let [{:keys [rows]} controls-map,
        row-count (count rows)]
    (cond-> controls-map
      (pos? row-count)
      ; remove rows
      (update-in [:rows] #(u/iteration n, pop, %)))))


(defn remove-rows
  [controls-map, show-header?, n]
  (cond-> controls-map
    show-header? (remove-headers :row, n)
    true (remove-text-cell-rows n)))


(defn remove-column-group-text-cells
  [controls-map, n]
  (let [columns (get-in controls-map [:columns, :sample-group-columns]),
        column-count (count columns)]
    (if (pos? column-count)
      (let [columns-to-remove (take-last n columns),
            ^GridPane grid-pane (:column-group-grid-pane controls-map)]
        ; remove column constraints
        (jfx/remove-grid-constraints grid-pane, :column, (- column-count n) column-count)
        ; remove controls
        (apply jfx/remove-children grid-pane columns-to-remove)
        ; remove group columns
        (update-in controls-map [:columns, :sample-group-columns] #(u/iteration n, pop, %)))
      ; return controls-map unmodified
      controls-map)))


(defn remove-columns
  [controls-map, show-header?, n]
  (cond-> controls-map
    show-header? (remove-headers :column, n)
    true (remove-column-group-text-cells n)))


(defn adjust-rows+columns
  "Add missing or remove superfluous rows or columns."
  [controls-map, show-header?, old-plot-layout, new-plot-layout]
  (let [visible-gene-count-delta (long (- (:alteration-row-count new-plot-layout) (:alteration-row-count old-plot-layout)))
        visible-sample-count-delta (long (- (:alteration-column-count new-plot-layout) (:alteration-column-count old-plot-layout)))]
    (cond-> controls-map
      (pos? visible-gene-count-delta) (add-rows show-header?, new-plot-layout, (Math/abs visible-gene-count-delta))
      (neg? visible-gene-count-delta) (remove-rows show-header?, (Math/abs visible-gene-count-delta))
      (pos? visible-sample-count-delta) (add-columns show-header?, new-plot-layout, (Math/abs visible-sample-count-delta))
      (neg? visible-sample-count-delta) (remove-columns show-header?, (Math/abs visible-sample-count-delta)))))


(defn max-view-positions
  [plot-layout, plot-data]
  (let [{:keys [alteration-row-count, alteration-column-count]} plot-layout,
        {:keys [sample-count, gene-list]} plot-data,
        gene-count (count gene-list)
        max-gene-pos (max (- gene-count alteration-row-count), 0)
        max-sample-pos (max (- sample-count alteration-column-count), 0)]
    {:max-gene-pos max-gene-pos,
     :max-sample-pos max-sample-pos,
     :gene-count gene-count,
     :sample-count sample-count}))


(defn resize-plot
  "Only performs changes if the available size for the plot changed and the number of visible rows or columns changed."
  [current-plot-map, new-plot-layout]
  (let [{:keys [state, controls, show-header?]} current-plot-map,
        {:keys [plot-data, plot-layout, view]} state]
    (if (and new-plot-layout plot-data (not= plot-layout new-plot-layout))
      (let [controls (adjust-rows+columns controls, show-header?, plot-layout, new-plot-layout),
            new-view (initial-view plot-data, new-plot-layout),
            {:keys [max-gene-pos,
                    max-sample-pos]} (max-view-positions new-plot-layout, plot-data)
            {:keys [first-gene-row-index, first-sample-column-index]} view]

        (-> current-plot-map
          (assoc :controls controls)
          (update-in [:state] assoc
            :plot-layout new-plot-layout,
            ; try to stay close to previous view position
            :view (assoc new-view
                    :first-gene-row-index (min first-gene-row-index, max-gene-pos),
                    :first-sample-column-index (min first-sample-column-index, max-sample-pos)))))
      ; return plot-map unmodified since no resize was possible
      current-plot-map)))


(defn update-view
  "Updates the view based on the plot-data assuming that the plot layout remains the same."
  [plot-map]
  (update-in plot-map [:state]
    (fn [{:keys [plot-data, plot-layout] :as state-map}]
      (let [{:keys [max-gene-pos,
                    max-sample-pos,
                    gene-count,
                    sample-count]} (max-view-positions plot-layout, plot-data)]
        (update-in state-map [:view]
          (fn [view]
            (-> view
              (update-in [:first-gene-row-index] min max-gene-pos)
              (update-in [:first-sample-column-index] min max-sample-pos)
              (assoc
                :sample-count sample-count,
                :gene-count gene-count))))))))


(defn update-grid-pane-constraint
  [grid-pane, constraint-type, length, constraint-index]
  (case constraint-type
    :row (-> grid-pane
           (jfx/get-row-constraint constraint-index)
           (jfx/row-constraint-update! (if (number? length)
                                         {:min-height length, :max-height length}
                                         {:fill-height? true})))
    :column (-> grid-pane
              (jfx/get-column-constraint constraint-index)
              (jfx/column-constraint-update! (if (number? length)
                                               {:min-width length, :max-width length}
                                               {:fill-width? true}))))
  grid-pane)


(defn create-grid-pane-constraint
  ([grid-pane, constraint-type, length, _]
   (create-grid-pane-constraint grid-pane, constraint-type, length))
  ([grid-pane, constraint-type, length]
   (case constraint-type
     :row (jfx/row-constraint! grid-pane, (if (number? length)
                                            {:min-height length, :max-height length}
                                            {:fill-height? true}))
     :column (jfx/column-constraint! grid-pane, (if (number? length)
                                                  {:min-width length, :max-width length}
                                                  {:fill-width? true})))
   grid-pane))


(defn adjust-root-grid-pane-constraints
  "Adjust the row and column sizes of the root-grid-pane of the plot."
  [controls-map,
   show-header?,
   {:keys [alteration-canvas-width,
           alteration-canvas-height,
           column-header-height,
           row-header-width,
           sample-group-height,
           gene-group-width,
           gene-name-width,
           alteration-ratio-width,
           scrollbar-width] :as plot-layout}]
  (let [{:keys [^GridPane root-grid-pane]} controls-map,
        constraint-fn (if (pos? (jfx/children-count root-grid-pane))
                        update-grid-pane-constraint
                        create-grid-pane-constraint)
        offset (if show-header? 1 0)]
    (when show-header?
      ; column header row
      (constraint-fn root-grid-pane, :row, column-header-height, 0)
      ; row header column
      (constraint-fn root-grid-pane, :column, row-header-width, 0))
    (-> root-grid-pane
      ; adjust row with column groups
      (constraint-fn :row, sample-group-height, (+ 0 offset))
      ; adjust alterations row
      (constraint-fn :row, alteration-canvas-height, (+ 1 offset))
      ; scrollbar row
      (constraint-fn :row, scrollbar-width, (+ 2 offset))
      ; filler row
      (constraint-fn :row, :fill, (+ 3 offset))
      ; adjust column with gene groups
      (constraint-fn :column, gene-group-width, (+ 0 offset))
      ; adjust column with gene names
      (constraint-fn :column, gene-name-width, (+ 1 offset))
      ; adjust alterations column
      (constraint-fn :column, alteration-canvas-width, (+ 2 offset))
      ; adjust column with alteration ratios
      (constraint-fn :column, alteration-ratio-width, (+ 3 offset))
      ; scrollbar column
      (constraint-fn :column, scrollbar-width, (+ 4 offset))
      ; filler column
      (constraint-fn :column, :fill, (+ 5 offset))))
  ; no return value, just side-effects
  nil)



(defn bind-scrollbar-value
  [^ScrollBar scrollbar, plot-ref, data-property-path]
  (let [data-prop (jfx/map-entry-property plot-ref, data-property-path)]
    ; listen to scrollbar value changes and update data value if needed
    (jfx/listen-to
      (fn [scrollbar-value]
        (let [scrollbar-value (long scrollbar-value)]
          ; scrollbar shall have only integer values
          (jfx/property-value! scrollbar, :value, scrollbar-value)
          ; update plot view
          (dosync
            (let [data-value (get-in (ensure plot-ref), data-property-path)]
              (when-not (= scrollbar-value data-value)
                (alter plot-ref assoc-in data-property-path scrollbar-value))))))
      (jfx/property scrollbar, :value))
    ; listen to data value changes and update scrollbar value if needed
    (jfx/listen-to
      (fn [data-value]
        (when data-value
          (let [scrollbar-value (jfx/property-value scrollbar, :value)]
            (when-not (= data-value scrollbar-value)
              (jfx/property-value! scrollbar, :value data-value)))))
      data-prop)))


(defn bind-scrollbar-max
  [^ScrollBar scrollbar, plot-ref, total-property-path, visible-property-path]
  (let [total-prop (jfx/map-entry-property plot-ref, total-property-path),
        visible-prop (jfx/map-entry-property plot-ref, visible-property-path)]
    (jfx/bind (jfx/property scrollbar, :max)
      (jfx/functional-property
        (fn [total, visible]
          (if (and total visible)
            (max (- total visible), 0.0)
            0.0))
        total-prop,
        visible-prop))
    (jfx/bind (jfx/property scrollbar, :visible-amount)
      (jfx/functional-property
        (fn [scrollbar-max, total, visible]
          (if (and scrollbar-max, total, visible)
            (* scrollbar-max (/ (double visible) total))
            0.1))
        (jfx/property scrollbar, :max),
        total-prop
        visible-prop))))


(defn setup-scrollbars
  [plot-ref, vertical-scrollbar, horizontal-scrollbar]
  ; vertical scrollbar (genes)
  (bind-scrollbar-value vertical-scrollbar, plot-ref, [:state, :view, :first-gene-row-index])
  (bind-scrollbar-max vertical-scrollbar, plot-ref, [:state, :view, :gene-count], [:state, :view, :visible-gene-count])
  ; horizontal scrollbar (samples)
  (bind-scrollbar-value horizontal-scrollbar, plot-ref, [:state, :view, :first-sample-column-index])
  (bind-scrollbar-max horizontal-scrollbar, plot-ref, [:state, :view, :sample-count], [:state, :view, :visible-sample-count]))


(defn maybe-setup-root-grid-pane-components
  [plot-ref, controls-map, show-header?, {:keys [alteration-row-height, sample-group-height] :as plot-layout}]
  (let [{:keys [root-grid-pane]} controls-map]
    (when-not (pos? (jfx/children-count root-grid-pane))
      (when show-header?
        (-> root-grid-pane
          ; setup static header buttons
          (jfx/add-to-grid 0, 0, (create-header-label alteration-row-height, nil))
          (jfx/add-to-grid 0, 1, (create-header-label alteration-row-height, "Group"))
          (jfx/add-to-grid 0, 2, (create-header-label alteration-row-height, "Gene"))
          (jfx/add-to-grid 0, 4, (-> (create-header-label alteration-row-height, "Alt.")
                                   (jfx/tooltip! "alteration percentage")))
          (jfx/add-to-grid 1, 0, (create-header-label sample-group-height, "Group"))
          ; setup header grid panes
          (jfx/add-to-grid 0, 3, (get-in controls-map [:headers, :column, :grid-pane]))
          (jfx/add-to-grid 2, 0, (get-in controls-map [:headers, :row, :grid-pane]))
          ))
      (let [offset (if show-header? 1 0)
            {:keys [horizontal-scrollbar,
                    vertical-scrollbar,
                    column-group-grid-pane,
                    row-group-grid-pane,
                    row-gene-grid-pane,
                    canvas,
                    export-button,
                    snapshot-button
                    statistics-button,
                    export-alteration-count-data-button
                    export-alteration-data-button
                    plot-menu,
                    row-alteration-ratio-grid-pane]} controls-map,
            {:keys [group-name-column-control
                    gene-name-column-control
                    alteration-ratio-column-control]} (:columns controls-map),
            export-plot-handler (pdata/action-handler plot-ref, :export-plot-handler)
            show-objectives-handler (pdata/action-handler plot-ref, :show-objectives-handler)
            snapshot-handler (pdata/action-handler plot-ref, :snapshot-handler)
            export-alteration-count-data-handler (pdata/action-handler plot-ref, :export-alteration-count-data-handler)
            export-alteration-data-handler (pdata/action-handler plot-ref, :export-alteration-data-handler)]
        (setup-scrollbars plot-ref, vertical-scrollbar, horizontal-scrollbar)
        (-> root-grid-pane
          ; child grid panes
          (jfx/add-to-grid (+ 0 offset), (+ 2 offset), column-group-grid-pane)
          (jfx/add-to-grid (+ 1 offset), (+ 0 offset), row-group-grid-pane)
          (jfx/add-to-grid (+ 1 offset), (+ 1 offset), row-gene-grid-pane)
          (jfx/add-to-grid (+ 1 offset), (+ 3 offset), row-alteration-ratio-grid-pane)
          ; canvas
          (jfx/add-to-grid (+ 1 offset), (+ 2 offset), canvas)
          ; scrollbars
          (jfx/add-to-grid (+ 1 offset), (+ 4 offset), vertical-scrollbar)
          (jfx/add-to-grid (+ 2 offset), (+ 2 offset), horizontal-scrollbar)
          ; add toolbox
          (jfx/add-to-grid (+ 2 offset), (+ 0 offset),
            (doto (jfx/column-span! plot-menu, 2)
              (->
                (props/items)
                (props/add-items
                  (cond-> []
                    ; statistics view
                    show-objectives-handler
                    (conj
                      (doto statistics-button
                        (jfx/set-menu-item-action!
                          (fn [_]
                            (show-objectives-handler (pdata/data plot-ref), (pdata/style plot-ref))))))
                    ; snapshot button
                    snapshot-handler
                    (conj
                      (doto snapshot-button
                        (jfx/set-menu-item-action!
                          (fn [_]
                            (snapshot-handler plot-ref)))))
                    ; separator
                    (and
                      (or show-objectives-handler snapshot-handler)
                      (or export-plot-handler export-alteration-count-data-handler export-alteration-data-handler))
                    (conj (SeparatorMenuItem.))
                    ; export button
                    export-plot-handler
                    (conj
                      (doto export-button
                        (jfx/set-menu-item-action!
                          (fn [_]
                            (export-plot-handler plot-ref)))))
                    ; export alteration count data button
                    export-alteration-count-data-handler
                    (conj
                      (doto export-alteration-count-data-button
                        (jfx/set-menu-item-action!
                          (fn [_]
                            (export-alteration-count-data-handler plot-ref)))))
                    ; export alteration data button
                    export-alteration-data-handler
                    (conj
                      (doto export-alteration-data-button
                        (jfx/set-menu-item-action!
                          (fn [_]
                            (export-alteration-count-data-handler plot-ref))))))))))
          ; static column controls
          (jfx/add-to-grid (+ 0 offset), (+ 0 offset), group-name-column-control)
          (jfx/add-to-grid (+ 0 offset), (+ 1 offset), gene-name-column-control)
          (jfx/add-to-grid (+ 0 offset), (+ 3 offset), alteration-ratio-column-control)))))
  ; no return value, just side-effects
  nil)



(defn add-constraints
  [constraint-type, controls-map, grid-pane-key, plot-layout, length-key, n]
  (let [grid-pane (get controls-map grid-pane-key)
        length (get plot-layout length-key)]
    (dotimes [_ n]
      ; add constraint
      (jfx/add-grid-constraint grid-pane, (jfx/grid-constraint-fixed-length constraint-type, length)))))


(defn show-rows
  [controls-map, grid-pane-key, control-key, row-index-offset, rows-to-add]
  (let [grid-pane (get controls-map grid-pane-key)]
    (u/for-each-indexed!
      (fn [i, row]
        (let [control (get row control-key)
              row-index (+ i row-index-offset)]
          ; add row control
          (jfx/add-to-grid grid-pane, row-index, 0, control)))
      rows-to-add)))


(defn remove-constraints
  [constraint-type, controls-map, grid-pane-key, from, to]
  (let [^GridPane grid-pane (get controls-map grid-pane-key)]
    ; remove constraints
    (jfx/remove-grid-constraints grid-pane, constraint-type, from, to)))


(defn hide-rows
  [controls-map, grid-pane-key, from, to]
  (let [^GridPane grid-pane (get controls-map grid-pane-key)]
    ; remove row controls
    (jfx/remove-children-by-indices grid-pane, from, to)))


(defn add-header-constraint
  [grid-pane, {:keys [alteration-row-height, alteration-cell-width] :as plot-layout}, header-type]
  (jfx/add-grid-constraint grid-pane,
    (jfx/grid-constraint-fixed-length header-type,
      (case header-type
        :row alteration-row-height
        :column alteration-cell-width))))


(defn show-headers
  [controls-map, plot-layout, header-type, header-index-offset, headers-to-add]
  (let [grid-pane (get-in controls-map [:headers, header-type, :grid-pane])]
    (u/for-each-indexed!
      (fn [i, header]
        (let [header-index (+ i header-index-offset)]
          ; add constraint
          (add-header-constraint grid-pane, plot-layout, header-type)
          ; add header
          (jfx/add-to-grid grid-pane,
            (case header-type
              :row header-index
              :column 0),
            (case header-type
              :row 0
              :column header-index),
            header)))
      headers-to-add)))


(defn hide-headers
  [controls-map, header-type, from, to]
  (let [^GridPane grid-pane (get-in controls-map [:headers, header-type, :grid-pane])]
    ; remove header
    (jfx/remove-children-by-indices grid-pane, from, to)
    ; remove constraint
    (jfx/remove-grid-constraints grid-pane, header-type, from, to)))


(defn hide-group-controls
  "Hide group controls in grid-pane that are not in the given list of controls."
  [grid-pane, controls]
  (let [control-set (set controls),
        controls-to-remove (remove control-set (jfx/children grid-pane))]
    (apply jfx/remove-children grid-pane, controls-to-remove)))


(defn event-handler
  [plot-map, event-handler-kw]
  (or
    (get-in plot-map [:event-handler-map, event-handler-kw])
    (partial pdata/missing-event-handler event-handler-kw)))



(defn setup-header-event-handling
  [plot-ref, event-handler-fn, header-type, header-offset, header-list]
  (u/for-each-indexed!
    (fn [i, header]
      (let [header-index (+ i header-offset)]
        (-> header
          (jfx/setup-event-handling!
            (jfx/drag-and-drop-event-handler-map (event-handler-fn :on-drag-and-drop), plot-ref, header-type, header-index))
          (jfx/handle-event! :mouse-clicked (event-handler-fn :on-header-clicked), plot-ref, header-type, header-index))))
    header-list))


(defn setup-row-event-handling
  [plot-ref, event-handler-fn, row-offset, rows-to-add]
  (u/for-each-indexed!
    (fn [i, {:keys [group-name-control, gene-name-control] :as row}]
      (let [row-index (+ i row-offset)]
        (jfx/handle-event! group-name-control, :mouse-clicked (event-handler-fn :on-text-cell-clicked) plot-ref, :row, row-index, :group)
        (jfx/handle-event! gene-name-control, :mouse-clicked (event-handler-fn :on-text-cell-clicked) plot-ref, :row, row-index, :gene)))
    rows-to-add))


(defn render-rows
  [plot-ref, event-handler-fn, {:keys [rows, row-gene-grid-pane] :as controls-map}, plot-layout, show-header?]
  (let [row-count (count rows)
        grid-row-count (jfx/children-count row-gene-grid-pane),
        delta (long (- row-count grid-row-count))]
    (cond
      (pos? delta) (do
                     (when show-header?
                       (let [headers-to-add (subvec (get-in controls-map [:headers, :row, :headers]), (- row-count delta) row-count)]
                         (setup-header-event-handling plot-ref, event-handler-fn, :row, grid-row-count, headers-to-add)
                         (show-headers controls-map, plot-layout, :row, grid-row-count, headers-to-add)))
                     (let [rows-to-add (subvec rows, (- row-count delta), row-count)
                           n-add (count rows-to-add)
                           add-constraints-fn (fn [grid-pane-key]
                                                (add-constraints :row, controls-map, grid-pane-key, plot-layout, :alteration-row-height, n-add))]
                       (setup-row-event-handling plot-ref, event-handler-fn, grid-row-count, rows-to-add)
                       ; groups are added during painting not here
                       ; but group row constraints must be added
                       (add-constraints-fn :row-group-grid-pane)
                       (add-constraints-fn :row-gene-grid-pane)
                       (show-rows controls-map, :row-gene-grid-pane, :gene-name-control, grid-row-count, rows-to-add)
                       (add-constraints-fn :row-alteration-ratio-grid-pane)
                       (show-rows controls-map, :row-alteration-ratio-grid-pane, :alteration-ratio-control, grid-row-count, rows-to-add)))

      (neg? delta) (do
                     (when show-header?
                       (hide-headers controls-map, :row, row-count, grid-row-count))
                     (let [remove-constraints-fn (fn [grid-pane-key]
                                                   (remove-constraints :row, controls-map, grid-pane-key, row-count, grid-row-count))]
                       ; remove superfluous group controls (due to resizing)
                       (hide-group-controls (:row-group-grid-pane controls-map), (mapv :group-name-control rows))
                       (remove-constraints-fn :row-group-grid-pane),
                       (hide-rows controls-map, :row-gene-grid-pane, row-count, grid-row-count)
                       (remove-constraints-fn :row-gene-grid-pane),
                       (hide-rows controls-map, :row-alteration-ratio-grid-pane, row-count, grid-row-count)
                       (remove-constraints-fn :row-alteration-ratio-grid-pane)))))
  ; no return value, just side-effects
  nil)


(defn setup-column-event-handling
  [plot-ref, event-handler-fn, column-offset, new-column-groups]
  (u/for-each-indexed!
    (fn [i, group-column-control]
      (let [column-index (+ i column-offset)]
        (jfx/handle-event! group-column-control, :mouse-clicked (event-handler-fn :on-text-cell-clicked) plot-ref, :column, column-index, :group)))
    new-column-groups))


(defn render-columns
  [plot-ref, event-handler-fn, {:keys [columns, column-group-grid-pane] :as controls-map}, plot-layout, show-header?]
  ; remove superfluous group controls (due to resizing)
  (let [{:keys [sample-group-columns]} columns
        column-count (count sample-group-columns),
        grid-column-count (count (jfx/grid-constraints column-group-grid-pane, :column))
        delta (long (- column-count grid-column-count))]
    (hide-group-controls column-group-grid-pane, sample-group-columns)
    (cond
      (pos? delta) (do
                     (add-constraints :column, controls-map, :column-group-grid-pane, plot-layout, :alteration-cell-width, delta)
                     (setup-column-event-handling plot-ref, event-handler-fn, grid-column-count, (subvec sample-group-columns (- column-count delta), column-count)))
      (neg? delta) (remove-constraints :column, controls-map, :column-group-grid-pane, column-count, grid-column-count)))
  (when show-header?
    (let [{:keys [headers, grid-pane]} (get-in controls-map [:headers, :column]),
          column-header-count (count headers),
          grid-column-header-count (jfx/children-count grid-pane)
          delta (long (- column-header-count grid-column-header-count))]
      (cond
        (pos? delta) (let [headers-to-add (subvec headers, (- column-header-count delta) column-header-count)]
                       (setup-header-event-handling plot-ref, event-handler-fn, :column, grid-column-header-count, headers-to-add)
                       (show-headers controls-map, plot-layout, :column, grid-column-header-count, headers-to-add))
        (neg? delta) (hide-headers controls-map, :column, column-header-count, grid-column-header-count))))
  ; no return value, just side-effects
  nil)


(defn adjust-canvas
  "Adjust the size of the alteration canvas."
  [controls-map, {:keys [alteration-canvas-width, alteration-canvas-height] :as plot-layout}]
  (let [{:keys [canvas]} controls-map]
    (jfx/canvas-size! canvas, alteration-canvas-width, alteration-canvas-height))
  ; no return value, just side-effects
  nil)


(defn update-column-constraints
  [controls-map, show-header?, {:keys [alteration-cell-width] :as plot-layout}]
  (when show-header?
    (let [{:keys [grid-pane, headers]} (get-in controls-map [:headers :column])]
      (jfx/update-all-grid-constraints-fixed-length grid-pane, :column, alteration-cell-width)
      (u/for-each-indexed!
        (fn [_, column-header]
          (-> column-header
            (jfx/min-width! alteration-cell-width)
            (jfx/max-width! alteration-cell-width)))
        headers)))
  (let [{:keys [column-group-grid-pane]} controls-map]
    (jfx/update-all-grid-constraints-fixed-length column-group-grid-pane, :column, alteration-cell-width)))


(defn update-row-constraints
  [controls-map, show-header?, {:keys [alteration-row-height] :as plot-layout}]
  (when show-header?
    (let [{:keys [grid-pane, headers]} (get-in controls-map [:headers :row])]
      (jfx/update-all-grid-constraints-fixed-length grid-pane, :row, alteration-row-height)))
  (doseq [grid-pane-key [:row-group-grid-pane, :row-gene-grid-pane, :row-alteration-ratio-grid-pane]]
    (let [grid-pane (get controls-map grid-pane-key)]
      (jfx/update-all-grid-constraints-fixed-length grid-pane, :row, alteration-row-height))))



(defn render-plot
  [plot-ref, {:keys [show-header?, controls], {:keys [plot-layout]} :state, :as plot-map}]
  (adjust-root-grid-pane-constraints controls, show-header?, plot-layout)
  (maybe-setup-root-grid-pane-components plot-ref, controls, show-header?, plot-layout)
  (update-column-constraints controls, show-header?, plot-layout)
  (update-row-constraints controls, show-header?, plot-layout)
  (let [event-handler-fn (partial event-handler plot-map)]
    (render-rows plot-ref, event-handler-fn, controls, plot-layout, show-header?)
    (render-columns plot-ref, event-handler-fn, controls plot-layout, show-header?))
  (adjust-canvas controls, plot-layout)
  ; no return value, just side-effects
  nil)