; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot
  (:require
    [clojure.data :as data]
    [avatar.ui.plot.structure :as struct]
    [avatar.ui.plot.painting :as paint]
    [avatar.ui.plot.selection :as select]
    [avatar.ui.plot.modification :as mod]
    [avatar.ui.plot.drag-and-drop :as dnd]
    [avatar.util :as u]
    [clj-jfx.core :as jfx]
    [avatar.ui.plot.data :as pdata]
    [clojure.string :as str]
    [avatar.ui.plot.layout :as lo]
    [avatar.algorithms.common :as co]
    [avatar.ui.svg-export :as svg]
    [avatar.ui.dialogs.progress-view :as progress]
    [avatar.ui.tools :as t]
    [avatar.data.gene-data :as gd])
  (:import
    (javafx.scene.layout StackPane Pane GridPane)
    (javafx.scene.input MouseEvent MouseButton ScrollEvent)
    (javafx.scene.control ToggleButton)
    (javafx.geometry Side)))



(defn on-header-context-menu
  [plot, header, header-type, header-view-index, in-selection?, selected?]
  ; there seems to be a JavaFX bug that after a drag & drop the following right click on the drag source leads to unselecting the source toggle button
  ; workaround: set selected state to what we know
  (when-not (= in-selection? selected?)
    (.setSelected ^ToggleButton header in-selection?))
  (when in-selection?
    (when-let [cm (pdata/context-menu plot, header-type)]
      (pdata/contextmenu-data! plot {:header header, :header-view-index header-view-index})
      (.show cm, header, Side/RIGHT, 0, 0))))


(defn on-header-clicked
  [^MouseEvent event, plot, header-type, header-view-index]
  (let [^ToggleButton header (.getSource event),
        selected? (.isSelected header),
        in-selection? (pdata/header-selected? plot, header-type, header-view-index)]
    (cond
      (= (.getButton event) MouseButton/PRIMARY) (select/on-header-selected plot, header-type, header-view-index, selected?)
      (= (.getButton event) MouseButton/SECONDARY) (on-header-context-menu plot, header, header-type, header-view-index, in-selection?, selected?))))


(defn row-context-menu
  [plot]
  (jfx/context-menu
    (mapv
      (fn [spec]
        (if (= spec :separator)
          :separator
          (let [[caption, handler] spec]
            [caption, (partial handler plot, :row)])))
      [["Change Group" mod/on-change-group]
       :separator
       ["Select Group" select/on-select-group]
       ["Deselect Group" select/on-deselect-group]
       ["Select All" select/on-select-all]
       ["Invert Selection" select/on-invert-selection]
       :separator
       ["Move to Top" mod/on-move-to-front]
       ["Show Selected Gene Names" (fn [plot, & _] (select/on-show-gene-names plot))]
       ["Fix Order" (partial mod/on-modify-fixed-order true)]
       ["Unfix Order" (partial mod/on-modify-fixed-order false)]
       :separator
       ["Delete Genes" mod/on-delete]])))


(defn column-context-menu
  [plot]
  (jfx/context-menu
    (mapv
      (fn [spec]
        (if (= spec :separator)
          :separator
          (let [[caption, handler] spec]
            [caption, (partial handler plot, :column)])))
      [["Change Group" mod/on-change-group]
       :separator
       ["Select Group" select/on-select-group]
       ["Deselect Group" select/on-deselect-group]
       ["Select All" select/on-select-all]
       ["Invert Selection" select/on-invert-selection]
       :separator
       ["Move to Left" mod/on-move-to-front]
       ["Show Selected Sample IDs" (fn [plot, & _] (select/on-show-sample-ids plot))]
       :separator
       ["Delete Samples" mod/on-delete]])))


(defn on-text-cell-clicked
  [^MouseEvent mouse-event, plot, header-type, header-index, cell-type]
  (let [source-node (.getSource mouse-event)]
    (when (and
            (= (.getButton mouse-event) MouseButton/PRIMARY)
            (>= (.getClickCount mouse-event) 2))
      (mod/on-text-cell-edit plot, header-type, header-index, cell-type, source-node)
      (.consume mouse-event))))


(defn modified-only-width-or-height?
  [old-plot-layout, new-plot-layout]
  (let [modified-in-new (second (data/diff old-plot-layout, new-plot-layout)),
        modified-attrs (set (keys modified-in-new))]
    (or
      (= modified-attrs #{:width})
      (= modified-attrs #{:height})
      (= modified-attrs #{:width, :height}))))



(defn maybe-resize
  ([current-plot-map]
    ; maybe we need to resize
   (let [{:keys [width, height]} (get-in current-plot-map [:state, :plot-layout])]
     (maybe-resize current-plot-map, width, height)))
  ([current-plot-map, width, height]
   (let [{:keys [state, show-header?]} current-plot-map
         {:keys [plot-data, plot-style, plot-layout]} state,
         new-plot-layout (lo/calculate-plot-layout plot-data, plot-style, show-header?, width, height)]
     (if (and
           new-plot-layout
           (not= plot-layout new-plot-layout)
           (not (modified-only-width-or-height? plot-layout, new-plot-layout)))
       (u/timing "resize"
         (struct/resize-plot current-plot-map, new-plot-layout))
       ; return plot map unmodified
       current-plot-map))))


(defn update-plot-view
  [plot-ref, new-plot-map]
  (u/timing "render-plot"
    (struct/render-plot plot-ref, new-plot-map))
  (u/timing "repaint-plot"
    (paint/repaint-plot new-plot-map)))



(defn scroll-vertically
  [plot-ref, ^long delta]
  (dosync
    (alter plot-ref update-in [:state, :view]
      (fn [{:keys [first-gene-row-index, visible-gene-count, gene-count] :as view}]
        (let [max-index (max (- gene-count visible-gene-count), 0)
              new-index (-> (+ first-gene-row-index delta)
                          (max 0)
                          (min max-index))]
          (assoc view :first-gene-row-index (long new-index)))))))


(defn scroll-horizontally
  [plot-ref, ^long delta]
  (dosync
    (alter plot-ref update-in [:state, :view]
      (fn [{:keys [first-sample-column-index, visible-sample-count, sample-count] :as view}]
        (let [max-index (max (- sample-count visible-sample-count), 0)
              new-index (-> (+ first-sample-column-index delta)
                          (max 0)
                          (min max-index))]
          (assoc view :first-sample-column-index (long new-index)))))))


(defn scrolling
  [plot-ref, ^ScrollEvent e]
  (let [delta (.getDeltaY e)
        factor (if (.isControlDown e) 10 1)]
    (if (.isShiftDown e)
      ; horizontal scrolling
      (cond
        (pos? delta) (scroll-horizontally plot-ref, (* factor -1))
        (neg? delta) (scroll-horizontally plot-ref, (* factor +1)))
      ; vertical scrolling
      (cond
        (pos? delta) (scroll-vertically plot-ref, (* factor -1))
        (neg? delta) (scroll-vertically plot-ref, (* factor +1))))
    (.consume e)))





(defn root-control
  [plot]
  (get-in (deref plot) [:controls, :root-control]))


(defn update-data!
  [plot, new-plot-data, new-plot-style]
  (dosync
    (alter plot
      (fn [plot-map]
        (-> plot-map
          (update-in [:state] assoc
            :plot-data new-plot-data,
            :group-map (pdata/group-map new-plot-data)
            :plot-style new-plot-style)
          struct/update-view
          maybe-resize)))
    (select/clear-header-selection! plot)))


(defn resize-plot
  [plot, width, height]
  (dosync
    (ref-set plot
      (maybe-resize (deref plot), width, height))
    (select/rerender-selection-overlays plot))
  plot)


(defn data
  [plot]
  (pdata/data plot))


(defn style
  [plot]
  (pdata/style plot))


(defn select-genes
  [plot, gene-list]
  (select/select-genes plot, gene-list))


(defn shown-alteration-type!
  [plot, alteration-type]
  (let [{:keys [shown-alteration-type] :as plot-style} (pdata/style plot)
        {:keys [alteration-type-set]} (pdata/data plot)]
    (assert (contains? alteration-type-set alteration-type) "Plot data must have alteration data of the specified type.")
    (pdata/style! plot, (assoc plot-style :shown-alteration-type alteration-type))
    plot))


(defn shown-alteration-type
  [plot]
  (:shown-alteration-type (pdata/style plot)))


(defn invert-selection
  [plot]
  (dosync
    (when-let [header-type (select/selected-header-type plot)]
      (select/on-invert-selection plot, header-type, nil))))


(defn clear-selection
  [plot]
  (select/clear-header-selection! plot))


(defn on-modify-fixed-order
  [plot, fixed?]
  (dosync
    (when-let [header-type (select/selected-header-type plot)]
      (when (= header-type :row)
        (mod/on-modify-fixed-order fixed?, plot, header-type, nil)))))


(defn on-change-group
  [plot]
  (dosync
    (when-let [header-type (select/selected-header-type plot)]
      (mod/on-change-group plot, header-type, nil))))


(defn on-delete
  [plot]
  (dosync
    (when-let [header-type (select/selected-header-type plot)]
      (mod/on-delete plot, header-type, nil))))


(defn on-move-to-front
  [plot]
  (dosync
    (when-let [header-type (select/selected-header-type plot)]
      (mod/on-move-to-front plot, header-type, nil))))


(defn on-show-selection
  [plot]
  (when-let [header-type (select/selected-header-type plot)]
    (case header-type
      :column (select/on-show-sample-ids plot)
      :row (select/on-show-gene-names plot))))


(defn export-plot-data
  [file, plot]
  (let [{:keys [shown-alteration-type]} (style plot),
        gene-sample-data (co/select-alteration-data shown-alteration-type, (data plot)),
        {:keys [sample-id-vec, sample-permutation, sample-group-map, gene-list]} gene-sample-data,
        sep (u/column-separator file),
        row->str (fn [row] (str/join sep, row))
        {:keys [column->sample]} sample-permutation,
        sample-id-row (list* "" "" (mapv #(u/quoted-str (nth sample-id-vec %)) column->sample)),
        sample-group-row (list* "" "" (mapv #(u/quoted-str (get sample-group-map %)) column->sample))]
    (->> gene-list
      (mapv
        (fn [{:keys [group, gene, alterations, missing-values] :as gene-data}]
          (->> column->sample
            (mapv
              (fn [sample]
                (if (contains? alterations sample)
                  "1"
                  (if (contains? missing-values sample)
                    ""
                    "0"))))
            (list* (u/quoted-str gene) (u/quoted-str group))
            row->str)))
      (list*
        (row->str sample-id-row)
        (row->str sample-group-row))
      (str/join "\n")
      (spit file))))


(defn export
  [plot]
  (when-let [file (jfx/choose-file (-> plot root-control jfx/stage), "Export plot", :save,
                    {:file-types (u/extension-map "pdf" "svg" "png", "tiff")})]
    (u/safe-future
      (progress/with-shown-indeterminate-progress (format "Exporting plot to \"%s\" ..." (.getName file))
        (svg/export-plot (data plot), (style plot), file)))))


(defn export-alteration-data
  [plot]
  (when-let [file (t/ask-for-text-export-file (-> plot root-control jfx/stage), "plot")]
    (export-plot-data file, plot)))


(defn export-alteration-contingency-tables
  [file, plot]
  (let [shown-alteration-type (shown-alteration-type plot),
        gene-sample-data (co/select-alteration-data shown-alteration-type, (data plot)),
        {:keys [columns, rows]} (gd/alteration-contingency-tables gene-sample-data),
        sep (u/column-separator file),
        row->str (fn [row] (str/join sep, row))
        column-row (list* (u/quoted-str "gene") (mapv u/quoted-str columns))]
    (->> rows
      (mapv
        (fn [{:keys [gene, contingency-table] :as gene-data}]
          (row->str
            (list*
              (u/quoted-str gene)
              contingency-table))))
      (list*
        (row->str column-row))
      (str/join "\n")
      (spit file))))


(defn export-contingency-tables
  [plot]
  (when-let [file (t/ask-for-text-export-file (-> plot root-control jfx/stage), "alteration count")]
    (u/safe-future
      (progress/with-shown-indeterminate-progress (format "Exporting alteration count data to \"%s\" ..." (u/file-name file))
        (export-alteration-contingency-tables file, plot)))))


(defn create-plot
  [plot-data, plot-style, width, height,
   {:keys [show-header?,
           drag-and-drop-handler,
           fix-rows-handler,
           move-to-front-handler
           multi-group-edit-handler,
           delete-genes-handler,
           disable-ui!,
           single-cell-edit-handler,
           snapshot-handler,
           show-objectives-handler,
           selection-change-handler,
           plot-ref] :as options}]
  (assert (some? plot-data) "Plot data is required to create a plot!")
  (assert (some? plot-style) "Plot style is required to create a plot!")
  (let [root-grid-pane (-> (GridPane.) jfx/use-computed-size!)
        overlay-pane (-> (Pane.) (jfx/mouse-transparent! true))
        root-control (-> (StackPane.)
                       (jfx/max-size! Double/MAX_VALUE, Double/MAX_VALUE)
                       (jfx/add-children [root-grid-pane, overlay-pane])),
        plot-state {:plot-data plot-data,
                    :group-map (pdata/group-map plot-data)
                    :plot-style plot-style,
                    :plot-layout nil,
                    :view {:gene-count nil,
                           :visible-gene-count nil,
                           :first-gene-row-index nil,
                           :sample-count nil,
                           :visible-sample-count nil,
                           :first-sample-column-index nil,
                           :alteration-cell-width nil,
                           :alteration-cell-height nil}},
        selection-state {:selected-header-map {:row #{}
                                               :column #{}}}
        plot (if plot-ref
               ; allows to use external ref
               (dosync
                 (ref-set plot-ref (with-meta {} {:type :plot}))
                 plot-ref)
               (ref (with-meta {} {:type :plot})))
        controls {:root-control root-control,
                  :root-grid-pane root-grid-pane,
                  :overlay-pane overlay-pane,
                  :row-group-grid-pane nil,
                  :column-group-grid-pane nil,
                  :vertical-scrollbar nil,
                  :horizontal-scrollbar nil,
                  :canvas nil,
                  :headers {:row {:grid-pane nil, :headers nil}
                            :column {:grid-pane nil, :headers nil}}
                  :rows []
                  :columns {:group-name-column-control nil,
                            :gene-name-column-control nil,
                            :sample-group-columns nil,
                            :alteration-ratio-column-control nil},
                  :selection-overlay-map {:row {},
                                          :column {},
                                          :row-group {},
                                          :column-group {}}
                  :context-menu-map {:row (row-context-menu plot),
                                     :column (column-context-menu plot)}}
        plot-map {:show-header? show-header?,
                  :action-handler-map {:fix-rows-handler fix-rows-handler,
                                       :move-to-front-handler move-to-front-handler,
                                       :multi-group-edit-handler multi-group-edit-handler,
                                       :delete-genes-handler delete-genes-handler,
                                       :drag-and-drop-handler drag-and-drop-handler,
                                       :disable-ui! disable-ui!,
                                       :single-cell-edit-handler single-cell-edit-handler,
                                       :export-plot-handler export,
                                       :export-alteration-data-handler export-alteration-data
                                       :export-alteration-count-data-handler export-contingency-tables,
                                       :snapshot-handler snapshot-handler,
                                       :show-objectives-handler show-objectives-handler},
                  :event-handler-map {:on-header-clicked on-header-clicked
                                      :on-drag-and-drop dnd/on-drag-and-drop,
                                      :on-text-cell-clicked on-text-cell-clicked},
                  :state plot-state,
                  :selection selection-state,
                  :controls controls
                  :context-menu-data nil},
        plot-map (struct/initialize plot-map, width, height)]
    ; first set plot-map (handler etc are then present for update-plot-view)
    (dosync
      (ref-set plot plot-map))

    ; setup mouse wheel scrolling on root control
    (jfx/filter-event! root-control, :scroll (partial scrolling plot))

    (update-plot-view plot, plot-map)

    (select/setup-selection-rendering plot, selection-change-handler)

    ; DEBUG
    #_(add-watch plot, :debug,
        (fn [_, _, old-plot-map, new-plot-map]
          (when-not (= old-plot-map new-plot-map)
            (println "\nplot changed:")
            (u/print-differences old-plot-map new-plot-map)
            ;(st/print-stack-trace (Exception. "tracing"))
            (flush))
          #_(let [old-plot-state (:state old-plot-map),
                  new-plot-state (:state new-plot-map)]
              (when-not (= old-plot-state new-plot-state)
                (u/print-modifications "\nstate changed" old-plot-state new-plot-state)
                (st/print-stack-trace (Exception. "tracing"))
                (flush)))))

    ; update watch
    (add-watch plot, :update,
      (fn [_, _, old-plot-map, new-plot-map]
        (let [old-plot-state (:state old-plot-map),
              new-plot-state (:state new-plot-map)]
          (when-not (= old-plot-state new-plot-state)
            #_(println "\nplot state changed:")
            #_(u/print-differences old-plot-state new-plot-state)
            (let [modified-in-new (u/flat-map-diff old-plot-state, new-plot-state)]
              ; only call handler if view, plot data or plot style changed
              (when (or (contains? modified-in-new :view) (contains? modified-in-new :plot-data) (contains? modified-in-new :plot-style))
                (update-plot-view plot, new-plot-map)))))))
    plot))