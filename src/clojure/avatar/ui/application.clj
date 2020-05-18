; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.application
  (:require
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [com.stuartsierra.component :as c]
    [clojure.tools.logging :as log]
    [avatar.util :as u]
    [clj-jfx.init] ; to get it AOT compiled by leiningen
    [clj-jfx.core :as jfx]
    [avatar.ui.dialogs.import-matrix-file :as import-matrix-dialog]
    [avatar.ui.dialogs.import-table-file :as import-table-dialog]
    [avatar.data.management :as dm]
    [avatar.data.operations :as ops]
    [avatar.ui.plot :as plot]
    [avatar.algorithms.score :as score]
    [avatar.algorithms.batch :as batch]
    [avatar.version :as v]
    [avatar.ui.dialogs.import-bioportal :as imp-bp]
    [avatar.ui.dialogs.import-geo :as imp-geo]
    [avatar.ui.dialogs.pareto-front-view :as pfv]
    [avatar.ui.dialogs.saved-pareto-fronts-view :as spfv]
    [avatar.ui.dialogs.batch-pareto-front-view :as bpfv]
    [avatar.ui.dialogs.delete-genes :as gmcd]
    [avatar.ui.dialogs.merge-group :as mgd]
    [avatar.ui.dialogs.gene-sort :as gsd]
    [avatar.ui.dialogs.gene-selection :as gsel]
    [avatar.ui.dialogs.settings :as settings]
    [avatar.ui.dialogs.group-samples :as gs]
    [avatar.ui.plot-export :as pexp]
    [avatar.ui.plot.data :as pdata]
    [avatar.algorithms.progress-tracker :as progress]
    [avatar.ui.dialogs.progress-view :as progress-view]
    [avatar.ui.dialogs.statistics-view :as sv]
    [avatar.ui.dialogs.single-optimization-settings :as single-opt-set]
    [avatar.ui.dialogs.batch-optimization-settings :as batch-opt-set]
    [avatar.ui.dialogs.import-clinical-data :as icd]
    [avatar.ui.dialogs.manage-groups :as mg]
    [avatar.ui.dialogs.manage-clinical-data :as mcd]
    [avatar.ui.dialogs.solution-comparison-view :as scv]
    [avatar.ui.dialogs.about-dialog :as about]
    [clojure.string :as str]
    [avatar.ui.dialogs.gene-symbol-normalization :as gsn]
    [avatar.data.gene-symbol-synonyms :as gss]
    [avatar.algorithms.common :as co]
    [avatar.ui.tools :as t]
    [clojure.set :as set]
    [avatar.data.gene-data :as gd]
    [avatar.data.file-conversion :as fc]
    [avatar.algorithms.mo-select :as mo-select]
    [clj-jfx.properties :as props])
  (:import
    (javafx.scene Scene Node)
    (javafx.scene.layout BorderPane)
    (javafx.stage Stage)
    (javafx.scene.control Button CheckMenuItem TableView Menu MenuItem RadioMenuItem)
    (java.lang Thread$UncaughtExceptionHandler)
    (javafx.application Platform)
    (javafx.beans.binding Bindings)
    (javafx.scene.input MouseEvent)
    (java.io FileNotFoundException)
    (javafx.geometry Bounds)
    (clojure.lang ExceptionInfo)
    (javafx.scene.image Image)))



(defn root-scene
  ^Scene [ui]
  (.getScene (t/primary-stage ui)))


(defn current-plot
  [ui]
  (:plot (deref (:controls-atom ui))))


(defn current-plot!
  [ui, plot]
  (swap! (:controls-atom ui) assoc :plot plot)
  ui)


(defn snapshot-view
  [ui]
  (:snapshot-view (deref (:controls-atom ui))))


(defn saved-pareto-fronts-view
  [ui]
  (:saved-pareto-fronts-view (deref (:controls-atom ui))))


(defn batch-pareto-fronts-view
  [ui]
  (:batch-pareto-fronts-view (deref (:controls-atom ui))))


(defn currently-shown-alteration-type
  [ui]
  (let [plot (current-plot ui)]
    (:shown-alteration-type (plot/style plot))))


(defn control-map
  [ui]
  (some-> ui :controls-atom deref :control-map))


(defn control
  [ui-or-map, control-id]
  (let [controls (cond-> ui-or-map (record? ui-or-map) control-map)]
    (get controls control-id)))


(defn install-uncaught-exception-handler
  []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_, thread, e]
        (u/log-exception "Uncaught exception in JavaFX thread", e)))))


(def ^:const border-spec {:widths 1.0, :stroke :solid, :color "#b6b6b6"})


(defn create-main-window
  []
  (jfx/run-now
    (install-uncaught-exception-handler)
    (let [control (jfx/create-control "avatar/ui/MainWindow.fxml")
          parent (jfx/control-node control)
          main-scene (Scene. parent, 1200, 600)]
      {:primary-stage (doto (Stage.)
                        (.setTitle "Avatar")
                        (.setScene main-scene)
                        (.setResizable true)
                        (t/add-avatar-icon))
       :control-map (jfx/control-children control)})))



(defn file-exists?
  [file]
  (.exists (io/file file)))


(defn global-data-filename
  []
  (u/file-in-home-folder ".avatar-data"))



(defn filter-existing-files
  [recent-files-data]
  (reduce-kv
    (fn [existing-files, filename, timestamp]
      (cond-> existing-files (file-exists? filename) (assoc filename timestamp)))
    {}
    recent-files-data))


(defn keep-newest
  [n, recent-files-data]
  (->> recent-files-data
    (sort-by val >)
    (take n)
    (into {})))


(defn newest-existing-files
  [recent-files-data]
  (->> recent-files-data
    filter-existing-files
    (keep-newest 10)))


(defn load-global-data
  []
  (let [fname (global-data-filename)]
    (when (file-exists? fname)
      (-> fname
        slurp
        edn/read-string
        (update-in [:recent-files] newest-existing-files)))))


(defn save-global-data
  [global-data]
  (let [fname (global-data-filename)]
    (with-open [f (io/writer fname)]
      (.write f (prn-str global-data)))))


(defn drag-and-drop-handler
  [ui, plot, header-type, source-index-list, target-index]
  (let [{:keys [data-management]} ui]
    (case header-type
      :row (u/safe-future (ops/move-genes data-management, source-index-list, target-index))
      :column (u/safe-future (ops/move-samples data-management, source-index-list, target-index)))))


(defn single-cell-edit-handler
  [ui, editable-type, element-index, result-mode, new-value]
  (when (= result-mode :accept-modified)
    (u/safe-future
      (case editable-type
        :gene-group (ops/rename-gene-group-block (:data-management ui), element-index, new-value)
        :sample-group (ops/rename-sample-group-block (:data-management ui), element-index, new-value)
        :gene-name (ops/rename-gene (:data-management ui), element-index, new-value)))))


(defn multi-group-edit-handler
  [ui, header-type, selected-indices, group-name]
  (u/safe-future
    (case header-type
      :row (ops/change-gene-groups (:data-management ui), selected-indices, group-name)
      :column (ops/change-sample-groups (:data-management ui), selected-indices, group-name))))


(defn delete-handler
  [ui, header-type, selected-indices]
  (u/safe-future
    (case header-type
      :row (ops/delete-selected-genes (:data-management ui), selected-indices)
      :column (ops/delete-samples-by-selected-columns (:data-management ui), selected-indices))))


(defn fix-rows-handler
  [ui, selected-rows, fix?]
  (u/safe-future (ops/modify-fixed-order-of-genes (:data-management ui), selected-rows, fix?)))


(defn move-to-front-handler
  [ui, header-type, selection]
  (u/safe-future (ops/move-to-front (:data-management ui), header-type, selection)))


(defn disable-menu+toolbar!
  [ui, disable?]
  (let [{:keys [menu, toolbar]} (control-map ui)]
    (jfx/disable! menu, disable?)
    (jfx/disable! toolbar, disable?))
  nil)


(defn bind-disabled
  [control-map, node-id, property]
  (if-let [node (get control-map, (-> node-id name keyword))]
    (jfx/bind (props/property node, :disable) property)
    (log/errorf "Node \"%s\" not found!" node-id))
  ; return primary stage for threading
  control-map)


(defn update-menu
  [ui, data-mgmt]
  (swap! (:menu-state ui) assoc
    :undo-disabled? (not (dm/may-undo? data-mgmt))
    :redo-disabled? (not (dm/may-redo? data-mgmt))
    :no-data? (not (dm/has-plot-data? data-mgmt))
    :no-snapshots? (not (dm/has-snapshots? data-mgmt))
    :no-latest-pareto-front? (not (dm/has-latest-pareto-front? data-mgmt))
    :no-saved-pareto-fronts? (not (dm/has-saved-pareto-fronts? data-mgmt))
    :no-batch-pareto-fronts? (not (dm/has-batch-pareto-fronts? data-mgmt))))


(defn setup-menu
  [ui]
  (let [menu-state (atom
                     (zipmap
                       [:undo-disabled?
                        :redo-disabled?
                        :no-data?
                        :no-snapshots?
                        :no-latest-pareto-front?
                        :no-saved-pareto-fronts?
                        :no-batch-pareto-fronts?]
                       (repeat true)))
        undo-disabled? (jfx/map-entry-property menu-state, :undo-disabled?),
        redo-disabled? (jfx/map-entry-property menu-state, :redo-disabled?),
        no-data? (jfx/map-entry-property menu-state, :no-data?),
        no-snapshots? (jfx/map-entry-property menu-state, :no-snapshots?)
        no-latest-pareto-front? (jfx/map-entry-property menu-state, :no-latest-pareto-front?)
        no-saved-pareto-fronts? (jfx/map-entry-property menu-state, :no-saved-pareto-fronts?)
        no-batch-pareto-fronts? (jfx/map-entry-property menu-state, :no-batch-pareto-fronts?)
        no-saved-solutions? (jfx/and-properties no-snapshots? no-latest-pareto-front? no-saved-pareto-fronts? no-batch-pareto-fronts?)]
    (jfx/run-now
      (-> (control-map ui)
        (bind-disabled "edit-undo", undo-disabled?)
        (bind-disabled "undo-button", undo-disabled?)
        (bind-disabled "edit-redo", redo-disabled?)
        (bind-disabled "redo-button", redo-disabled?)
        (bind-disabled "edit-menu", no-data?)
        (bind-disabled "selection-menu", no-data?)
        (bind-disabled "view-menu", no-data?)
        (bind-disabled "groups-menu", no-data?)
        (bind-disabled "sort-menu", no-data?)
        (bind-disabled "optimization-menu", no-data?)
        (bind-disabled "snapshots-menu", no-data?)
        (bind-disabled "edit-delete-genes-without-alterations", no-data?)
        (bind-disabled "edit-delete-genes", no-data?)
        (bind-disabled "edit-delete-samples-without-alterations", no-data?)
        (bind-disabled "edit-delete-samples-without-clinical-data", no-data?)
        (bind-disabled "edit-insert-bioportal-study", no-data?)
        (bind-disabled "edit-insert-geo-dataset", no-data?)
        (bind-disabled "edit-insert-alteration-data-from-file-menu", no-data?)
        (bind-disabled "edit-insert-matrix-data-file", no-data?)
        (bind-disabled "edit-insert-table-data-file", no-data?)
        (bind-disabled "edit-insert-clinical-attributes", no-data?)
        (bind-disabled "groups-merge-group-alterations", no-data?)
        (bind-disabled "groups-group-samples-by-clinical-attribute", no-data?)
        (bind-disabled "edit-normalize-gene-names", no-data?)
        (bind-disabled "edit-manage-clinical-data", no-data?)
        (bind-disabled "groups-manage-sample-groups", no-data?)
        (bind-disabled "groups-manage-gene-groups", no-data?)
        (bind-disabled "selection-select-genes", no-data?)
        (bind-disabled "file-save", no-data?)
        (bind-disabled "file-save-as", no-data?)
        (bind-disabled "file-export-plot", no-data?)
        (bind-disabled "file-export-plot-data", no-data?)
        (bind-disabled "file-export-clinical-data", no-data?)
        (bind-disabled "file-settings", no-data?)
        (bind-disabled "view-plot-data-statistics", no-data?)
        (bind-disabled "sort-genes", no-data?)
        (bind-disabled "sort-genes-by-group", no-data?)
        (bind-disabled "sort-genes-by-alteration-count", no-data?)
        (bind-disabled "sort-samples", no-data?)
        (bind-disabled "sort-samples-without-groups", no-data?)
        (bind-disabled "optimization-single-optimization", no-data?)
        (bind-disabled "optimization-batch-optimization", no-data?)
        (bind-disabled "optimization-view-latest-gene-selections", no-latest-pareto-front?)
        (bind-disabled "optimization-view-saved-pareto-fronts", no-saved-pareto-fronts?)
        (bind-disabled "optimization-view-batch-optimization-results", no-batch-pareto-fronts?)
        (bind-disabled "optimization-compare-solutions", no-saved-solutions?)
        (bind-disabled "snapshots-save", no-data?))
      (-> (snapshot-view ui)
        (jfx/find-node "save-snapshot-button")
        (props/property :disable)
        (jfx/bind no-data?)))
    (assoc ui :menu-state menu-state)))


(defn update-view-menu
  [ui, {:keys [alteration-type-set] :as plot-state}, {:keys [shown-alteration-type] :as plot-style}]
  (let [alteration-type->control {:mutation-data :view-mutation-data,
                                  :expression-data :view-expression-data,
                                  :methylation-data :view-methylation-data}
        control-map (control-map ui)
        view-menu-items (reduce
                          #(assoc %1 %2 (get control-map, %2))
                          {}
                          (vals alteration-type->control))]
    ; update enabled/disabled
    (u/for-each-kv!
      (fn [atype, control-kw]
        (let [^MenuItem control (get view-menu-items control-kw)]
          (jfx/property-value! control :disable (not (contains? alteration-type-set atype)))))
      alteration-type->control)
    ; update selected menu item
    (when (and shown-alteration-type (contains? alteration-type-set shown-alteration-type))
      (let [^RadioMenuItem menu-item (->> shown-alteration-type
                                       (get alteration-type->control)
                                       (get view-menu-items))]
        (jfx/property-value! menu-item, :selected true)))))


(defn fixed-row?
  [plot, row-index]
  (let [{:keys [gene-list]} (plot/data plot)]
    (pdata/fixed-gene? (nth gene-list row-index))))


(defn update-title
  [ui, data-mgmt]
  (-> ui t/primary-stage
    (.setTitle
      (if-let [file (dm/current-file data-mgmt)]
        (format "Avatar - %s%s" (.getName file) (if (dm/dirty? data-mgmt) "*" ""))
        "Avatar"))))


(defn objectives-map
  [sample-count, gene-list]
  (when sample-count
    (let [{:keys [coverage, overlap]} (-> (score/total-overlap+coverage :alterations, gene-list)
                                        (u/update-map-vals #(/ % (double sample-count))))
          fixed? (some :order-fixed? gene-list),
          notfixed-genes (when fixed? (remove :order-fixed? gene-list))
          {:keys [notfixed-coverage, notfixed-overlap]} (when (seq notfixed-genes)
                                                          (-> (score/total-overlap+coverage :alterations, notfixed-genes)
                                                            (u/update-map-vals #(/ % (double sample-count)))
                                                            (set/rename-keys
                                                              {:coverage :notfixed-coverage
                                                               :overlap :notfixed-overlap})))]
      {:coverage coverage, :overlap overlap,
       :coverage-without-fixed notfixed-coverage, :overlap-without-fixed notfixed-overlap})))


(defn update-statusbar
  [ui, gene-sample-data, {:keys [shown-alteration-type] :as plot-style}]
  (if gene-sample-data
    (let [{:keys [sample-count, gene-list]} (co/select-alteration-data shown-alteration-type, gene-sample-data)]
      (swap! (:statusbar-state ui)
        #(-> %
           (assoc
             :gene-count (count gene-list),
             :sample-count sample-count,
             :shown-alteration-type (gd/alteration-type-str shown-alteration-type))
           (merge
             (objectives-map sample-count, gene-list)))))
    ; no data -> no stats
    (reset! (:statusbar-state ui)
      {:gene-count nil,
       :sample-count nil,
       :shown-alteration-type nil,
       :coverage nil,
       :coverage-without-fixed nil,
       :overlap nil,
       :overlap-without-fixed nil})))

(defn setup-statusbar
  [ui]
  (let [statusbar-state (atom
                          {:gene-count nil,
                           :sample-count nil,
                           :shown-alteration-type nil,
                           :coverage nil,
                           :coverage-without-fixed nil,
                           :overlap nil,
                           :overlap-without-fixed nil})
        bind-text (fn [controls, node-id, & property-kw+fmt-list]
                    (if-let [node (get controls (-> node-id name keyword))]
                      (let [kws (vec (take-nth 2 property-kw+fmt-list))
                            fmts (vec (take-nth 2 (rest property-kw+fmt-list)))]
                        (jfx/bind (props/property node, :text)
                          (apply jfx/functional-property
                            (fn [& values]
                              (let [formatted-strs (->> values
                                                     ; formated strings only for non-nil values
                                                     (map
                                                       (fn [fmt, val]
                                                         (when (some? val)
                                                           (format fmt val)))
                                                       fmts)
                                                     ; remove nil values
                                                     (remove nil?))]
                                (if (seq formatted-strs)
                                  (apply str formatted-strs)
                                  "N/A")))
                            ; get properties
                            (map (partial jfx/map-entry-property statusbar-state) kws))))
                      (log/errorf "Node \"%s\" not found!" node-id))
                    controls)]
    (jfx/run-now
      (-> (control-map ui)
        (bind-text "gene-count-label" :gene-count "%d")
        (bind-text "sample-count-label" :sample-count "%d")
        (bind-text "shown-alteration-type-label" :shown-alteration-type "%s")
        (bind-text "coverage-label" :coverage "%.3f" :coverage-without-fixed " (%.3f)")
        (bind-text "overlap-label" :overlap "%.3f" :overlap-without-fixed " (%.3f)")))
    (assoc ui :statusbar-state statusbar-state)))


(defn selection-change-handler
  [ui, selected-header-map]
  (let [controls (control-map ui)
        selection-label (:selection-label controls)
        header-type (reduce-kv
                      (fn [_, type, selection-set]
                        (when (seq selection-set)
                          (reduced type)))
                      nil
                      selected-header-map),
        selection-str (if header-type
                        (let [n (count (get selected-header-map header-type))]
                          (str
                            n
                            " "
                            (case header-type
                              :row "gene"
                              :column "sample")
                            (when (> n 1)
                              "s")
                            " selected"))
                        "")
        nothing-selected? (nil? header-type)]
    (jfx/label-text! selection-label, selection-str)
    (doseq [menu-item-id ["selection-invert-selection"
                          "selection-clear-selection"
                          "selection-move-to-front"
                          "selection-show-selected"
                          "selection-fix-order"
                          "selection-unfix-order"
                          "selection-change-group"
                          "selection-delete-selection"]]
      (when-let [^MenuItem menu-item (get controls, (-> menu-item-id name keyword))]
        (jfx/property-value! menu-item, :disable, nothing-selected?)))))


(defn clear-ui
  [ui]
  (when-let [plot (current-plot ui)]
    (let [{:keys [plot-parent]} (control-map ui)]
      (jfx/clear-children plot-parent)
      (current-plot! ui, nil))))


(defn update-plot
  [ui, data-mgmt, old-state, new-plot-state, new-plot-style]
  (if (and (nil? old-state) (nil? new-plot-state))
    (jfx/run-later (clear-ui ui))
    (jfx/run-later
      (update-menu ui, data-mgmt)
      (update-view-menu ui, new-plot-state, new-plot-style)
      (update-title ui, data-mgmt)
      (update-statusbar ui, new-plot-state, new-plot-style)
      (when new-plot-state
        (if-let [plot (current-plot ui)]
          ; repaint plot completely (possibility to update plot incrementally)
          (plot/update-data! plot, new-plot-state, new-plot-style)
          ; create new plot
          (let [{:keys [plot-parent]} (control-map ui)
                {:keys [width, height]} (-> plot-parent jfx/layout! jfx/size)
                plot (plot/create-plot
                       new-plot-state,
                       new-plot-style,
                       width,
                       height,
                       {:show-header? true,
                        :drag-and-drop-handler (partial drag-and-drop-handler ui),
                        :single-cell-edit-handler (partial single-cell-edit-handler ui),
                        :multi-group-edit-handler (partial multi-group-edit-handler ui),
                        :delete-genes-handler (partial delete-handler ui),
                        :fixed-row? fixed-row?,
                        :fix-rows-handler (partial fix-rows-handler ui),
                        :move-to-front-handler (partial move-to-front-handler ui),
                        :snapshot-handler (partial pexp/save-plot-as-snapshot data-mgmt),
                        :show-objectives-handler (partial sv/show-objectives-view data-mgmt),
                        :selection-change-handler (partial selection-change-handler ui),
                        :disable-ui! (partial disable-menu+toolbar! ui)})]
            (current-plot! ui, plot)
            (-> plot-parent
              jfx/clear-children
              (jfx/add-child (plot/root-control plot)))
            (plot/update-data! plot, new-plot-state, new-plot-style)))))))


(defn add-data-change-handler
  [ui]
  (update-in ui [:data-management] #(dm/add-change-handler %, (partial update-plot ui))))


(defn on-window-resize
  [ui, ^BorderPane content-pane, ^long width, ^long, height]
  (when-let [plot (current-plot ui)]
    (let [left-width (or (some-> content-pane (jfx/property-value :left) jfx/width) 0.0),
          right-width (or (some-> content-pane (jfx/property-value :right) jfx/width) 0.0),
          top-height (or (some-> content-pane (jfx/property-value :top) jfx/height) 0.0),
          bottom-height (or (some-> content-pane (jfx/property-value :bottom) jfx/height) 0.0),
          plot-width (- width left-width right-width),
          plot-height (- height top-height bottom-height)]
      (plot/resize-plot plot, plot-width, plot-height))))


(defn add-window-resize-handler
  [ui]
  (let [{:keys [^BorderPane content-pane]} (control-map ui)]
    (jfx/change-listener! (jfx/property content-pane, :layout-bounds)
      (fn [_, _, ^Bounds layout-bounds]
        (on-window-resize ui, content-pane, (.getWidth layout-bounds), (.getHeight layout-bounds)))))
  ui)


(defn update-recent-files
  [{:keys [global-data-atom] :as ui}, file]
  (let [filename (.getCanonicalPath (io/file file))
        timestamp (System/currentTimeMillis)]
    (swap! global-data-atom
      update-in
      [:recent-files]
      (fn [recent-file-map]
        (newest-existing-files (assoc recent-file-map filename timestamp))))))


(defn save-to-file
  [ui, file]
  (u/safe-future
    (try
      (progress-view/with-shown-indeterminate-progress (format "Saving data to \"%s\" ..." (u/file-name file))
        (dm/save-to-file (:data-management ui), file)
        (update-recent-files ui, file))
      (catch FileNotFoundException e
        (u/log-exception "Saving file failed.", e)
        (jfx/show-error "Save File Error", "Saving file failed.", (.getMessage e)))
      (catch Throwable t
        (u/log-exception "Saving file failed.", t)
        (jfx/show-error "Save File Error", "Saving file failed.", "Unexpected error.")))))




(defn save-as
  [ui, initial-filename]
  (when-let [file (jfx/choose-file (t/primary-stage ui), "Save file", :save,
                    {:file-types {"Avatar file (*.avatar)" ["*.avatar"]}
                     :initial-file initial-filename})]
    (save-to-file ui, file)))

(defn on-file-save-as
  [ui, event]
  (save-as ui, nil))


(defn on-file-save
  [ui, event]
  (if-let [file (dm/current-file (:data-management ui))]
    (if (str/ends-with? (str file) ".iggy")
      (save-as ui, (str/replace (str file) #"\.iggy$" ".avatar"))
      (save-to-file ui, file))
    (on-file-save-as ui, nil)))


(defn handle-unsaved-changes
  [{:keys [data-management] :as ui}, context]
  (when (and
          (dm/dirty? data-management)
          (jfx/ask-confirmation
            "Unsaved changes",
            (format "Do you want to save the current data before %s?" context),
            "There are unsaved changes which will be lost permanently."))
    (on-file-save ui, nil)))


(defn import-file-matrix-data
  [ui, title, completion-fn]
  (when-let [file (jfx/choose-file (t/primary-stage ui),
                    title,
                    :open,
                    {:file-types {"Excel files" ["*.xls" "*.xlsx"],
                                  "Text files" ["*.csv", "*.tsv", "*.ssv", "*.txt"]}})]
    (import-matrix-dialog/show-dialog ui, file, completion-fn)))


(defn on-file-import-matrix
  [ui, event]
  (handle-unsaved-changes ui, "import")
  (u/safe-future
    (import-file-matrix-data ui, "Import alteration matrix"
      (fn [gene-sample-data]
        (dm/import-from-csv (:data-management ui), gene-sample-data)))))


(defn import-file-table-data
  [ui, title, completion-fn]
  (when-let [file (jfx/choose-file (t/primary-stage ui),
                    title,
                    :open,
                    {:file-types {"Excel files" ["*.xls" "*.xlsx"],
                                  "Text files" ["*.csv", "*.tsv", "*.ssv", "*.txt"]}})]
    (import-table-dialog/show-dialog ui, file, completion-fn)))


(defn on-file-import-table
  [ui, event]
  (handle-unsaved-changes ui, "import")
  (u/safe-future
    (import-file-table-data ui, "Import alteration table"
      (fn [gene-sample-data]
        (dm/import-from-csv (:data-management ui), gene-sample-data)))))


(defn on-file-import-bioportal-study
  [ui, event]
  (handle-unsaved-changes ui, "import")
  (imp-bp/show-dialog ui,
    (fn [{:keys [alteration-type-set] :as gene-sample-data}]
      (if (seq alteration-type-set)
        (dm/import-from-csv (:data-management ui), gene-sample-data)
        (jfx/show-information
          "Import Bioport Study",
          "No alteration data could be imported."
          nil)))))


(defn ask-for-duplicate-sample-handling
  [duplicate-sample-ids, complete-fn]
  (jfx/run-later
    (let [control (jfx/create-control "avatar/ui/SampleDuplicateHandlingDialog.fxml")
          control-node (jfx/control-node control),
          {:keys [duplicate-count-text
                  overwrite-button
                  append-only-button
                  apply-button
                  cancel-button] :as children} (jfx/control-children control)
          window (jfx/modal-window "Duplicate Sample IDs found", control-node)
          _ (jfx/toggle-group overwrite-button, append-only-button)
          selected-option-prop (jfx/selected-toggle-property {:overwrite overwrite-button, :append-only append-only-button})]

      (props/set-property duplicate-count-text :text (str (count duplicate-sample-ids)))

      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (complete-fn (props/get selected-option-prop))
          (jfx/close window)))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      (jfx/show window))))


(defn handle-duplicate-samples
  [{data-mgmt :data-management, :as ui}
   new-gene-sample-data]
  (if-let [duplicate-sample-ids (ops/problematic-duplicate-sample-ids data-mgmt, new-gene-sample-data)]
    (ask-for-duplicate-sample-handling duplicate-sample-ids,
      (fn [chosen-action]
        (ops/insert-gene-sample-data-append-or-overwrite data-mgmt, new-gene-sample-data, (= chosen-action :append-only))))
    (ops/insert-gene-sample-data-unconstrained data-mgmt, new-gene-sample-data)))



(defn on-edit-insert-bioportal-study
  [ui, event]
  (imp-bp/show-dialog ui,
    (fn [{:keys [alteration-type-set] :as gene-sample-data}]
      (if (seq alteration-type-set)
        (handle-duplicate-samples ui, gene-sample-data)
        (jfx/show-information
          "Insert Bioport Study",
          "No alteration data could be imported.",
          nil)))))


(defn on-file-import-geo-dataset
  [ui, event]
  (handle-unsaved-changes ui, "import")
  (imp-geo/show-dialog ui,
    (fn [{:keys [alteration-type-set] :as gene-sample-data}]
      (if (seq alteration-type-set)
        (dm/import-from-csv (:data-management ui), gene-sample-data)
        (jfx/show-information
          "Import GEO dataset",
          "No alteration data could be imported."
          nil)))))


(defn on-edit-insert-geo-dataset
  [ui, event]
  (imp-geo/show-dialog ui,
    (fn [{:keys [alteration-type-set] :as gene-sample-data}]
      (if (seq alteration-type-set)
        (handle-duplicate-samples ui, gene-sample-data)
        (jfx/show-information
          "Insert GEO dataset",
          "No alteration data could be imported.",
          nil)))))


(defn on-edit-insert-matrix-data-file
  [{:keys [data-management] :as ui}, event]
  (u/safe-future
    (import-file-matrix-data ui, "Insert data from alteration matrix"
      (fn [gene-sample-data]
        (if (and
              (ops/same-sample-set? data-management, gene-sample-data)
              (jfx/ask-confirmation "Insert Alteration Matrix",
                "Merge alteration data?"
                "The imported data has exactly the same sample ids as the current data.\nDo you want to merge the data?"))
          ; merge data of samples
          (ops/insert-gene-sample-data-merge-samples data-management, gene-sample-data)
          ; do not merge: treat as additional samples
          (when-let [sample-group (imp-bp/show-sample-group-dialog nil)]
            (ops/insert-gene-sample-data-unconstrained data-management, gene-sample-data, sample-group)))))))


(defn on-edit-insert-table-data-file
  [{:keys [data-management] :as ui}, event]
  (u/safe-future
    (import-file-table-data ui, "Insert data from alteration table",
      (fn [gene-sample-data]
        (if (and
              (ops/same-sample-set? data-management, gene-sample-data)
              (jfx/ask-confirmation "Insert Alteration Table",
                "Merge alteration data?"
                "The imported data has exactly the same sample ids as the current data.\nDo you want to merge the data?"))
          ; merge data of samples
          (ops/insert-gene-sample-data-merge-samples data-management, gene-sample-data)
          ; do not merge: treat as additional samples
          (when-let [sample-group (imp-bp/show-sample-group-dialog nil)]
            (ops/insert-gene-sample-data-unconstrained data-management, gene-sample-data, sample-group)))))))


(defn on-edit-insert-clinical-attributes
  [{:keys [data-management] :as ui}, event]
  (u/safe-future
    (when-let [file (jfx/choose-file (t/primary-stage ui),
                      "Import clinical data",
                      :open,
                      {:file-types {"Excel files" ["*.xls" "*.xlsx"],
                                    "Text files" ["*.csv", "*.tsv", "*.ssv", "*.txt"]}})]
      (let [{:keys [sample-id-vec]} (dm/current-data data-management)
            sample-id-set (-> sample-id-vec set (disj nil))]
        (icd/show-dialog ui, file, sample-id-set,
          (fn [clinical-data]
            (ops/add-clinical-data data-management, clinical-data)))))))


(defn on-groups-merge-group-alterations
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (when-let [{:keys [group-genes, result-gene] :as result} (mgd/show-dialog data-management, gene-sample-data, (dm/plot-style data-management))]
      (ops/merge-group data-management, group-genes, result-gene))))


(defn on-groups-group-samples-by-clinical-attribute
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (gs/show-dialog gene-sample-data,
      (fn [attribute]
        (ops/assign-sample-groups-by-clinical-attribute data-management, attribute, (currently-shown-alteration-type ui))))))


(defn on-groups-manage-groups
  [{:keys [data-management] :as ui}, group-type, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (mg/show-dialog ui, group-type, (gd/group->size-map group-type, gene-sample-data),
      (fn [group-renaming-map]
        (case group-type
          :sample (ops/rename-sample-groups data-management, group-renaming-map)
          :gene (ops/rename-gene-groups data-management, group-renaming-map))))))


(defn on-edit-normalize-gene-names
  [{:keys [data-management, synonym-service] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (u/safe-future
      (when-let [gene-symbol-synonym-data (gss/get-gene-symbol-synonym-data synonym-service)]
        (gsn/show-dialog gene-symbol-synonym-data, (:gene-list gene-sample-data),
          (fn [renaming-map]
            (ops/rename-chosen-genes data-management, renaming-map)))))))


(defn on-edit-manage-clinical-data
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (mcd/show-dialog gene-sample-data,
      (fn [attribute-renaming, value-renaming-per-attribute]
        (ops/rename-clinical-attributes-and-values data-management, attribute-renaming, value-renaming-per-attribute)))))


(defn on-file-export-plot
  [ui, event]
  (plot/export (current-plot ui)))


(defn on-file-export-plot-data
  [ui, event]
  (when-let [file (t/ask-for-text-export-file (t/primary-stage ui), "plot")]
    (plot/export-plot-data file, (current-plot ui))))


(defn on-file-export-clinical-data
  [ui, event]
  (when-let [file (t/ask-for-text-export-file (t/primary-stage ui), "clinical")]
    (ops/export-clinical-data (:data-management ui), file)))


(defn update-optimization-setup
  [{:keys [global-data-atom] :as ui}, settings]
  (swap! global-data-atom assoc :optimization-setup settings))


(defn previous-optimization-settings
  [{:keys [global-data-atom] :as ui}]
  (:optimization-setup (deref global-data-atom)))


(defn load-from-file
  [ui, file]
  (u/safe-future
    (try
      ; upgrade file if saved with previous incompatible program version
      (when-not (= (fc/maybe-convert-file file) :file-not-converted)
        (progress-view/with-shown-indeterminate-progress (format "Loading data from \"%s\" ..." (u/file-name file))
          (dm/load-from-file (:data-management ui), file)))
      (catch ExceptionInfo e
        (jfx/show-error "Error: Opening file", (format "Loading \"%s\" failed." (u/file-absolute-path file)), (.getMessage e)))
      (catch Throwable t
        (let [msg (format "An unexpected error occured when trying to load \"%s\"." (u/file-absolute-path file))]
          (u/log-exception msg, t)
          (jfx/show-error "Error: Opening file", msg, nil))))
    (update-recent-files ui, file)))


(defn on-file-open
  [ui, event]
  (handle-unsaved-changes ui, "opening another file")
  (when-let [file (jfx/choose-file (t/primary-stage ui), "Open file", :open,
                    {:file-types
                     {"Avatar file (*.avatar)" ["*.avatar"]
                      "Iggy file (*.iggy)" ["*.iggy"]}})]
    (load-from-file ui, file)))


(defn content-border-pane
  ^BorderPane [ui]
  (control ui, :content-pane))


(defn show-or-hide-snapshots-view
  [ui, observable, old-value, new-value]
  (when-not (= old-value new-value)
    (let [content-pane (content-border-pane ui)
          ^Bounds layout-bounds (jfx/property-value content-pane, :layout-bounds),
          snapshot-view (snapshot-view ui)]
      (jfx/autosize snapshot-view)
      (if new-value
        (.setRight content-pane snapshot-view)
        (.setRight content-pane nil))
      (on-window-resize ui, content-pane, (.getWidth layout-bounds), (.getHeight layout-bounds)))))


(defn save-snapshot
  [ui]
  (when-let [snapshot-name (jfx/text-input "Save Snapshot", "Save the current plot", "Enter the snapshot name:")]
    (dm/save-snapshot (:data-management ui), snapshot-name)))


(defn load-snapshot
  [ui, ^TableView snapshot-table]
  (when-let [{:keys [id]} (jfx/selected-row snapshot-table)]
    (dm/load-snapshot (:data-management ui), id)))


(defn view-snapshot
  [ui, ^TableView snapshot-table]
  (let [data-mgmt (:data-management ui)]
    (when-let [{:keys [name, id]} (jfx/selected-row snapshot-table)]
      (let [{:keys [data, shown-alteration-type] :as snapshot} (dm/snapshot-by-id data-mgmt, id)]
        (sv/show-objectives-view data-mgmt, data,
          (assoc (dm/plot-style data-mgmt)
            :shown-alteration-type shown-alteration-type))))))


(defn delete-snapshot
  [ui, ^TableView snapshot-table]
  (when-let [{:keys [name, id]} (jfx/selected-row snapshot-table)]
    (when (jfx/ask-confirmation "Delete snapshot?",
            (format "Do you really want to delete the snapshot \"%s\"?" name),
            "This can not be undone.")
      (dm/delete-snapshot (:data-management ui), id))))


(defn snapshot-table-clicked
  [ui, ^MouseEvent e, snapshot-table]
  (when (jfx/double-click? e)
    (.consume e)
    (load-snapshot ui, snapshot-table)))


(defn snapshots-view-visible!
  [ui, visible?]
  (let [^Node view (snapshot-view ui)]
    (.setVisible view visible?)))


(defn create-snapshotview
  [snapshots-ref, {:keys [table-row-click, save-snapshot, load-snapshot, delete-snapshot, view-snapshot, close-button?, close-button-handler, show-or-hide], :as event-handlers}]
  (jfx/run-now
    (let [control (jfx/create-control "avatar/ui/SnapshotsView.fxml")
          ^BorderPane view (jfx/control-node control)
          {:keys [^TableView snapshot-table
                  save-snapshot-button
                  load-snapshot-button
                  view-statistics-button
                  delete-snapshot-button
                  snapshot-view-close-button]} (jfx/control-children control)
          column-keys [:id, :name, :created]]
      (when table-row-click
        (jfx/handle-event! snapshot-table :mouse-clicked table-row-click, snapshot-table))
      ; setup snapshot table
      (jfx/setup-table-view! snapshot-table, column-keys, snapshots-ref,
        :placeholder "No saved snapshots."
        :sort? true)
      ; save snapshot button
      (when save-snapshot
        (jfx/handle-event! save-snapshot-button, :action, (fn [_] (save-snapshot))))
      ; load snaphot button
      (when load-snapshot
        (jfx/handle-event! load-snapshot-button, :action, (fn [_] (load-snapshot snapshot-table))))
      ; view snapshot
      (when view-snapshot
        (jfx/handle-event! view-statistics-button, :action, (fn [_] (view-snapshot snapshot-table))))
      ; delete snapshot button
      (when delete-snapshot
        (jfx/handle-event! delete-snapshot-button, :action, (fn [_] (delete-snapshot snapshot-table))))
      (when snapshot-view-close-button
        (when-not close-button?
          (jfx/remove-children (jfx/parent snapshot-view-close-button) snapshot-view-close-button))
        (when (and close-button? close-button-handler)
          (jfx/handle-event! snapshot-view-close-button, :action, close-button-handler)))
      (doto view
        (.setVisible false)
        (cond-> show-or-hide (-> .visibleProperty (jfx/change-listener! show-or-hide)))
        (jfx/region-border! border-spec)))))


(defn create-global-snapshotview
  [ui, data-management, control-map]
  (jfx/run-now
    (let [snapshots-ref (dm/snapshots-ref data-management),
          view (create-snapshotview snapshots-ref,
                 {:table-row-click (partial snapshot-table-clicked ui),
                  :save-snapshot (partial save-snapshot ui)
                  :load-snapshot (partial load-snapshot ui)
                  :delete-snapshot (partial delete-snapshot ui),
                  :view-snapshot (partial view-snapshot ui)
                  :show-or-hide (partial show-or-hide-snapshots-view ui)
                  ; hide view on close button click
                  :close-button-handler (fn [_] (snapshots-view-visible! ui, false))
                  :close-button? true}),
          ^CheckMenuItem snapshots-view-menu-item (:snapshots-view control-map)]
      ; bind menu to visible property
      (Bindings/bindBidirectional (.selectedProperty snapshots-view-menu-item), (.visibleProperty ^Node view))
      view)))


(defn search-optimal-genes
  [ui, setup-data]
  (let [data-mgmt (:data-management ui),
        current-data (dm/current-data data-mgmt),
        progress-tracker (when (:show-progress? setup-data)
                           (let [{:keys [iteration-count, population-size]} setup-data]
                             (doto (progress-view/show-progress "Searching Optimal Gene Selections", (* iteration-count population-size))
                               (progress/start))))
        pareto-front-result (try
                              (mo-select/multi-objective-gene-selection current-data,
                                (cond-> setup-data
                                  progress-tracker
                                  (assoc :progress-tracker progress-tracker)))
                              (finally
                                (progress/finished progress-tracker)))]
    (dm/latest-pareto-front! data-mgmt, pareto-front-result)
    (pfv/pareto-front-view ui, (dm/plot-style data-mgmt), pareto-front-result)))


(defn on-sort-genes
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (gsd/show-dialog ui, gene-sample-data, (dm/plot-style data-management),
      (fn [sorted-gene-sample-data, selected-alteration-type]
        (when sorted-gene-sample-data
          (dm/modify-current-data data-management (constantly sorted-gene-sample-data))
          (dm/shown-alteration-type! data-management, selected-alteration-type))))))


(defn on-sort-genes-by-group
  [ui, event]
  (ops/sort-genes-by-group (:data-management ui), (currently-shown-alteration-type ui)))


(defn on-sort-genes-by-alteration-count
  [ui, event]
  (ops/on-sort-genes-by-alteration-count (:data-management ui), (currently-shown-alteration-type ui)))


(defn on-optimization-single-optimization
  [ui, event]
  (let [data-mgmt (:data-management ui)]
    (when-let [gene-sample-data (dm/current-data data-mgmt)]
      (single-opt-set/show-dialog gene-sample-data, (previous-optimization-settings ui),
        (fn [{:keys [settings, setup]}]
          (u/safe-future
            (update-optimization-setup ui, setup)
            (search-optimal-genes ui, settings)
            (update-menu ui, data-mgmt)))))))


(defn on-optimization-batch-optimization
  [ui, event]
  (let [data-mgmt (:data-management ui)]
    (if-let [snapshots (dm/snapshots data-mgmt)]
      ; TODO: retrieve previous optimization settings?
      (batch-opt-set/show-dialog snapshots, nil,
        (fn [batch-config]
          (u/safe-future
            ; TODO: store settings here?
            (let [batch (batch/execute-batch snapshots, batch-config, progress-view/show-multiple-progresses)
                  runs-with-errors (filterv :error (:pareto-front-list batch))]
              (dm/save-batch data-mgmt, (update-in batch [:pareto-front-list] (partial u/removev :error)))
              (update-menu ui, data-mgmt)
              (when (seq runs-with-errors)
                (jfx/show-errors-detailed "Batch execution errors"
                  (format "%s errors occured in the execution of batch \"%s\"" (count runs-with-errors) (:name batch))
                  (mapv :error runs-with-errors)))))))
      (jfx/show-information "Batch Optimization",
        "Batch optimization requires at least on snapshot!"
        "Batch optimization is based on snapshots that provide the different data inputs for the optimization runs."))))


(defn on-optimization-view-latest-gene-selections
  [ui, event]
  (let [{:keys [data-management]} ui
        pareto-front (dm/latest-pareto-front data-management)]
    (when pareto-front
      (pfv/pareto-front-view ui, (dm/plot-style data-management), pareto-front))))


(defn on-optimization-view-saved-pareto-fronts
  [ui, event]
  (let [view (saved-pareto-fronts-view ui)]
    (if (jfx/shown? view)
      (jfx/to-front view)
      (jfx/show view))))


(defn on-optimization-view-batch-optimization-results
  [ui, event]
  (let [view (batch-pareto-fronts-view ui)]
    (if (jfx/shown? view)
      (jfx/to-front view)
      (jfx/show view))))


(defn on-view-plot-data-statistics
  [ui, event]
  (let [data-mgmt (:data-management ui)]
    (when-let [gene-sample-data (dm/current-data data-mgmt)]
      (sv/show-objectives-view data-mgmt, gene-sample-data, (dm/plot-style data-mgmt)))))


(defn on-optimization-compare-genes
  [ui, event]
  (scv/show-dialog ui))


(defn on-sort-samples
  [ui, sample-group-first?, event]
  (ops/sort-columns (:data-management ui), sample-group-first?, (currently-shown-alteration-type ui)))


(defn on-edit-undo
  [ui, event]
  (dm/undo (:data-management ui)))


(defn on-edit-redo
  [ui, event]
  (dm/redo (:data-management ui)))


(defn on-selection-select-genes
  [{:keys [data-management] :as ui}, event]
  (when-let [plot (current-plot ui)]
    (when-let [gene-sample-data (plot/data plot)]
      (gsel/show-dialog ui, gene-sample-data, (dm/plot-style data-management)
        (fn [{:keys [action, gene-list]}]
          (if (= action :select-genes)
            (plot/select-genes plot, gene-list)
            ; other actions
            (let [gene-indices (gd/find-gene-indices (:gene-list gene-sample-data), (set gene-list))]
              (case action
                :set-gene-group (when-let [group-name (jfx/text-input "Change group", "Enter the new group name for the selected genes.", nil)]
                                  (ops/change-gene-groups data-management, gene-indices, group-name))
                :move-genes-to-top (ops/move-to-front data-management, :row, gene-indices)
                :fix-genes (ops/modify-fixed-order-of-genes data-management, gene-indices, true)
                :filter-genes (ops/filter-selected-genes data-management, gene-indices)
                :remove-genes (ops/delete-selected-genes data-management, gene-indices)))))))))


(defn on-selection-invert-selection
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/invert-selection plot)))


(defn on-selection-clear-selection
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/clear-selection plot)))


(defn on-selection-move-to-front
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/on-move-to-front plot)))


(defn on-selection-show-selected
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/on-show-selection plot)))


(defn on-modify-fixed-order
  [ui, fixed?, event]
  (when-let [plot (current-plot ui)]
    (plot/on-modify-fixed-order plot, fixed?)))


(defn on-selection-change-group
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/on-change-group plot)))


(defn on-selection-delete-selection
  [ui, event]
  (when-let [plot (current-plot ui)]
    (plot/on-delete plot)))


(defn on-view-alteration-type
  [ui, alteration-type, event]
  (dm/shown-alteration-type! (:data-management ui), alteration-type))


(defn on-edit-delete-genes-without-alterations
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (when-let [selected-alteration-types (t/alteration-selection-for-deletion-dialog gene-sample-data,
                                           "Delete genes without alterations", :gene)]
      (ops/delete-genes-without-alterations data-management, selected-alteration-types))))


(defn on-edit-delete-genes
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (when-let [{:keys [minimal-alterations, maximal-alterations, alteration-type-set]}
               (gmcd/show-dialog gene-sample-data, (currently-shown-alteration-type ui), :relative)]
      (ops/delete-genes data-management, minimal-alterations, maximal-alterations, alteration-type-set))))


(defn on-edit-delete-samples-without-alterations
  [{:keys [data-management] :as ui}, event]
  (when-let [gene-sample-data (dm/current-data data-management)]
    (when-let [selected-alteration-types (t/alteration-selection-for-deletion-dialog gene-sample-data,
                                           "Delete samples without alterations", :sample)]
      (ops/delete-samples-without-alterations data-management, selected-alteration-types))))


(defn on-edit-delete-samples-without-clinical-data
  [{:keys [data-management] :as ui}, event]
  (ops/delete-samples-without-clinical-data data-management))


(defn on-snapshots-save
  [ui, event]
  (snapshots-view-visible! ui, true)
  (save-snapshot ui))


(defn on-file-settings
  [ui, event]
  (settings/show-dialog ui))


(defn on-help-about
  [ui, event]
  (about/show-dialog ui))


(defn add-menu-event-handlers
  [ui]
  (let [^Stage stage (t/primary-stage ui),
        scene (.getScene stage)]
    (jfx/menu-item-actions! scene,
      {"file-import-matrix" (partial on-file-import-matrix ui),
       "file-import-table" (partial on-file-import-table ui)
       "file-import-bioportal-study" (partial on-file-import-bioportal-study ui),
       "file-import-geo-dataset" (partial on-file-import-geo-dataset ui),
       "file-export-plot" (partial on-file-export-plot ui),
       "file-export-plot-data" (partial on-file-export-plot-data ui),
       "file-export-clinical-data" (partial on-file-export-clinical-data ui),
       "file-open" (partial on-file-open ui),
       "file-save" (partial on-file-save ui),
       "file-save-as" (partial on-file-save-as ui),
       "file-settings" (partial on-file-settings ui),
       "file-quit" (fn [_] (.close stage)),
       "edit-undo" (partial on-edit-undo ui),
       "edit-redo" (partial on-edit-redo ui),
       "edit-delete-genes-without-alterations" (partial on-edit-delete-genes-without-alterations ui),
       "edit-delete-genes" (partial on-edit-delete-genes ui),
       "edit-delete-samples-without-alterations" (partial on-edit-delete-samples-without-alterations ui),
       "edit-delete-samples-without-clinical-data" (partial on-edit-delete-samples-without-clinical-data ui),
       "edit-insert-bioportal-study" (partial on-edit-insert-bioportal-study ui),
       "edit-insert-geo-dataset" (partial on-edit-insert-geo-dataset ui)
       "edit-insert-matrix-data-file" (partial on-edit-insert-matrix-data-file ui),
       "edit-insert-table-data-file" (partial on-edit-insert-table-data-file ui),
       "edit-insert-clinical-attributes" (partial on-edit-insert-clinical-attributes ui),
       "edit-normalize-gene-names" (partial on-edit-normalize-gene-names ui),
       "edit-manage-clinical-data" (partial on-edit-manage-clinical-data ui),
       "groups-merge-group-alterations" (partial on-groups-merge-group-alterations ui),
       "groups-group-samples-by-clinical-attribute" (partial on-groups-group-samples-by-clinical-attribute ui),
       "groups-manage-gene-groups" (partial on-groups-manage-groups ui, :gene)
       "groups-manage-sample-groups" (partial on-groups-manage-groups ui, :sample)
       "selection-select-genes" (partial on-selection-select-genes ui),
       "selection-invert-selection" (partial on-selection-invert-selection ui),
       "selection-clear-selection" (partial on-selection-clear-selection ui),
       "selection-move-to-front" (partial on-selection-move-to-front ui),
       "selection-show-selected" (partial on-selection-show-selected ui),
       "selection-fix-order" (partial on-modify-fixed-order ui, true),
       "selection-unfix-order" (partial on-modify-fixed-order ui, false),
       "selection-change-group" (partial on-selection-change-group ui),
       "selection-delete-selection" (partial on-selection-delete-selection ui),
       "view-mutation-data" (partial on-view-alteration-type ui, :mutation-data),
       "view-expression-data" (partial on-view-alteration-type ui, :expression-data),
       "view-methylation-data" (partial on-view-alteration-type ui, :methylation-data),
       "view-plot-data-statistics" (partial on-view-plot-data-statistics ui),
       "optimization-compare-solutions" (partial on-optimization-compare-genes ui),
       "sort-genes" (partial on-sort-genes ui),
       "sort-samples" (partial on-sort-samples ui, true),
       "sort-samples-without-groups" (partial on-sort-samples ui, false)
       "sort-genes-by-group" (partial on-sort-genes-by-group ui),
       "sort-genes-by-alteration-count" (partial on-sort-genes-by-alteration-count ui),
       "optimization-single-optimization" (partial on-optimization-single-optimization ui),
       "optimization-batch-optimization" (partial on-optimization-batch-optimization ui),
       "optimization-view-latest-gene-selections" (partial on-optimization-view-latest-gene-selections ui),
       "optimization-view-saved-pareto-fronts" (partial on-optimization-view-saved-pareto-fronts ui),
       "optimization-view-batch-optimization-results" (partial on-optimization-view-batch-optimization-results ui),
       "snapshots-save" (partial on-snapshots-save ui),
       "help-about" (partial on-help-about ui)})
    (jfx/button-actions! scene,
      {"undo-button" (partial on-edit-undo ui),
       "redo-button" (partial on-edit-redo ui)}))
  ui)



(defn recent-file-menu-item
  [ui, filename]
  (doto (MenuItem. (-> filename io/file .getName))
    ; set to false, otherwise "_" are swallowed
    (.setMnemonicParsing false)
    (jfx/set-menu-item-action!
      (fn [_]
        (handle-unsaved-changes ui, "opening another file")
        (load-from-file ui, (io/file filename))
        ; push to most recent
        (update-recent-files ui, (io/file filename))))))


(defn on-global-data-changed
  [ui,
   {old-recent-files :recent-files :as old-global-data},
   {new-recent-files :recent-files :as new-global-data}]
  ; save global data to file
  (save-global-data new-global-data)
  ; refresh recent files menu if needed
  (when-not (= old-recent-files new-recent-files)
    (let [^Menu file-open-recent-menu (control ui, :file-open-recent-menu),
          recent-files? (pos? (count new-recent-files)),
          recent-files-menu-items (.getItems file-open-recent-menu)]
      (.setDisable file-open-recent-menu (not recent-files?))
      (.clear recent-files-menu-items)
      (when recent-files?
        (doseq [filename (->> new-recent-files (sort-by val >) (map key))]
          (.add recent-files-menu-items (recent-file-menu-item ui, filename)))))))


(defn on-main-window-closing
  [{:keys [data-management] :as ui}, window-event]
  (handle-unsaved-changes ui, "closing"))


(defrecord UserInterface [data-management, implicit-exit?, splashscreen, controls-atom, global-data-atom, geo-download-directory, menu-state, statusbar-state]

  c/Lifecycle

  (start [ui]
    (jfx/implicit-exit! implicit-exit?)
    (try
      (let [{:keys [primary-stage, control-map]} (create-main-window)
            primary-stage (cond-> primary-stage
                            true
                            (jfx/handle-event! :window-hiding (partial on-main-window-closing ui))
                            implicit-exit?
                            ; trigger shutdown-agents when closing main window to prevent the program to hang (foreground vs background threads)
                            (jfx/handle-event! :window-hidden (fn [_] (shutdown-agents) (Platform/exit)))
                            ; splashscreen hiding
                            splashscreen
                            (jfx/handle-event! :window-shown
                              (fn [_]
                                (try
                                  (jfx/run-now
                                    (jfx/hide splashscreen))
                                  (finally)))))]
        (jfx/run-now
          (jfx/show primary-stage))
        (reset! controls-atom
          {:primary-stage primary-stage
           :snapshot-view (create-global-snapshotview ui, data-management, control-map)
           :saved-pareto-fronts-view (spfv/create-dialog ui)
           :batch-pareto-fronts-view (bpfv/create-dialog ui)
           :control-map control-map
           :plot nil})
        (add-watch global-data-atom :global-data-changed (fn [_, _, old-global-data, new-global-data] (on-global-data-changed ui, old-global-data, new-global-data)))
        (reset! global-data-atom (load-global-data))
        (-> ui
          setup-menu
          setup-statusbar
          add-data-change-handler
          add-window-resize-handler
          add-menu-event-handlers))
      (catch Throwable t
        (u/log-exception "Exception during application start.", t)
        (when splashscreen
          (jfx/run-now
            (jfx/hide splashscreen)))
        (jfx/show-error "Avatar Startup Error", "An unexpected error occured during application start.", nil)
        (when implicit-exit?
          (shutdown-agents)
          (Platform/exit)))))

  (stop [ui]
    (let [{:keys [primary-stage]} (deref controls-atom)]
      (when primary-stage
        (jfx/run-now
          (jfx/hide primary-stage)
          (jfx/close-all-windows)))
      (reset! controls-atom nil))
    (remove-watch global-data-atom :global-data-changed)
    (cond-> ui
      menu-state (assoc :menu-state nil)
      statusbar-state (assoc :statusbar-state nil))))


(defn create-user-interface
  [ui-config-map]
  (map->UserInterface
    (assoc ui-config-map
      :controls-atom (atom nil)
      :global-data-atom (atom nil))))