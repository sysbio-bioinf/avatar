; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.import-bioportal
  (:require
    [avatar.data.bioportal :as bp]
    [clj-jfx.core :as jfx]
    [clojure.string :as str]
    [avatar.ui.dialogs.import-matrix-file :as imp-dia]
    [clojure.set :as set]
    [avatar.util :as u]
    [avatar.ui.dialogs.progress-view :as progress]
    [avatar.ui.tools :as t]
    [avatar.data.import :as imp]
    [avatar.data.operations :as ops]
    [clojure.stacktrace :as st]
    [clj-jfx.selection :as jfx-select]
    [clojure.data.int-map :as im]
    [avatar.algorithms.common :as c]
    [avatar.data.gene-data :as gd])
  (:import
    (javafx.scene.control TextField TableView Label Button Toggle RadioButton CheckBox)
    (javafx.scene.chart BarChart XYChart$Data XYChart$Series)
    (javafx.scene Node)
    (javafx.scene.layout VBox HBox)))



(defn data-type->str
  [data-type]
  (case data-type
    :mutation-data "mutation"
    :expression-data "expression"))


(defn show-sample-group-dialog
  [initial-group-name]
  (let [sample-group-atom (atom {:sample-group initial-group-name}),
        result-atom (atom nil),
        control (jfx/create-control "avatar/ui/SampleGroupNameDialog.fxml"),
        control-node (jfx/control-node control),
        {:keys [^TextField sample-group-textfield,
                ok-button,
                cancel-button]} (jfx/control-children control),
        window (doto (jfx/modal-window "Specify sample group", control-node)
                 (t/add-avatar-icon))]
    (jfx/bind-bidirectional (jfx/property sample-group-textfield, :text), (jfx/map-entry-property sample-group-atom, :sample-group))
    (jfx/handle-event! cancel-button, :action, (fn [_] (jfx/close window)))
    (jfx/handle-event! ok-button, :action,
      (fn [_]
        (reset! result-atom (:sample-group (deref sample-group-atom)))
        (jfx/close window)))
    (jfx/show-and-wait window)
    (deref result-atom)))

(defn threshold-disabled-property
  [dialog-state, threshold-mode-set]
  (jfx/functional-property
    (fn [threshold-mode]
      (not (contains? threshold-mode-set threshold-mode)))
    (jfx/map-entry-property dialog-state, [:threshold-mode])))


(defn setup-expression-threshold-textfield
  [dialog-state, threshold-property-kw, error-property-kw, disabled-prop, ^TextField textfield]
  (let [text-prop (jfx/property textfield, :text),
        threshold-prop (jfx/map-entry-property dialog-state, [threshold-property-kw])]
    (jfx/listen-to
      (fn [threshold, disabled?]
        (jfx/value! text-prop,
          (when-not disabled?
            (str threshold))))
      threshold-prop
      disabled-prop)
    ; propagate changes to dialog-state
    (jfx/listen-to
      (fn [text, disabled?]
        (swap! dialog-state
          (fn [state]
            (if disabled?
              (assoc state
                error-property-kw nil)
              (if (u/is-double? text)
                (assoc state
                  threshold-property-kw text
                  error-property-kw nil)
                (assoc state
                  error-property-kw "must be a floating point number"))))))
      text-prop
      disabled-prop)))


(defn setup-histogram
  [^BarChart histogram-chart, histogram-data-prop]
  (let [series-list (jfx/observable-array-list)]
    (jfx/property-value! histogram-chart, :data, series-list)
    (jfx/listen-to
      (fn [histogram-data]
        (let [series (->> histogram-data
                       (sort-by :order)
                       (mapv #(XYChart$Data. (str (:x %)), (:y %)))
                       jfx/observable-array-list
                       XYChart$Series.)]
          (doto series-list
            jfx/clear-list
            (jfx/add-to-list series))))
      histogram-data-prop)))


(defn extract-expression-thresholds
  "Extract selected thresholds from dialog state."
  [{:keys [threshold-mode, upper-threshold, lower-threshold]}]
  (cond-> {}
    (#{:differentially-expressed, :overexpressed} threshold-mode)
    (assoc :upper-threshold (u/parse-double upper-threshold))
    (#{:differentially-expressed, :underexpressed} threshold-mode)
    (assoc :lower-threshold (u/parse-double lower-threshold))))


(defn create-expressed?-fn
  [{:keys [lower-threshold, upper-threshold]}]
  (if lower-threshold
    (if upper-threshold
      ; both thresholds
      (fn lower+upper-check [z-score]
        (and z-score (not (Double/isNaN z-score)) (or (<= z-score lower-threshold) (>= z-score upper-threshold))))
      ; only lower threshold
      (fn lower-check [z-score]
        (and z-score (not (Double/isNaN z-score)) (<= z-score lower-threshold))))
    (when upper-threshold
      ;only upper threshold
      (fn upper-check [z-score]
        (and z-score (not (Double/isNaN z-score)) (>= z-score upper-threshold))))))


(defn create-methylated?-fn
  [{:keys [threshold, methylation-mode] :as data}]
  (case methylation-mode
    :hypermethylated (fn hypermethylated-check [value]
                       (and value (not (Double/isNaN value)) (<= threshold value)))
    :hypomethylated (fn hypomethylated-check [value]
                      (and value (not (Double/isNaN value)) (>= threshold value)))))


(defn group-to-bins
  [bin-count, histogram-map]
  (let [values (keys histogram-map),
        max-value (reduce max values),
        value-range (inc (long max-value)),
        ; keep potential double-valued bin-size (works in quotient)
        bin-size (double (max (/ value-range bin-count), 1.0))]
    (->> histogram-map
      ; group into bins
      (reduce-kv
        (fn [bin-map, value, count]
          (u/update-in! bin-map [(long (quot value bin-size))] + count))
        ; every bin starts with count = 0
        (reduce #(assoc! %1 %2 0) (transient {}) (range bin-count)))
      persistent!
      ; create bin labels
      (reduce-kv
        (fn [histogram-par-vec, bin-id, count]
          (conj! histogram-par-vec
            {:order bin-id,
             :x (let [from (long (* bin-id bin-size)),
                      to (-> bin-id inc (* bin-size) long dec)]
                  (if (= from to)
                    (str from)
                    (format "%s - %s" from to)))
             :y count}))
        (transient []))
      persistent!)))


(defn calculate-histogram-data
  [bin-count, altered?-fn, values-per-gene]
  (when altered?-fn
    (->> values-per-gene
      (mapv
        (fn [{:keys [values]}]
          (reduce
            (fn [altered-count, zscore]
              (cond-> altered-count (altered?-fn zscore) inc))
            0
            values)))
      frequencies
      (group-to-bins bin-count))))


(def ^:const histogram-bin-count 30)


(defn ask-for-expression-thresholds
  [{:keys [values-per-gene] :as expression-data}]
  (jfx/run-now
    (let [dialog-state (atom {:lower-threshold (u/format-double -1.0),
                              :upper-threshold (u/format-double 1.0),
                              :lower-threshold-error nil,
                              :upper-threshold-error nil,
                              :threshold-mode nil,
                              :histogram-data nil}),
          result-promise (promise)
          control (jfx/create-control "avatar/ui/SelectExpressionThresholdsDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [histogram-chart,
                  ^Label
                  lower-threshold-label,
                  lower-threshold-textfield,
                  ^Label
                  lower-threshold-error-label,
                  ^Label
                  upper-threshold-label,
                  upper-threshold-textfield,
                  ^Label
                  upper-threshold-error-label,
                  ^RadioButton differentially-expressed-radiobutton,
                  overexpressed-radiobutton,
                  underexpressed-radiobutton,
                  ^Button
                  apply-button]
           :as children} (jfx/control-children control),
          toggle-group (.getToggleGroup ^Toggle differentially-expressed-radiobutton),
          lower-threshold-disabled-prop (threshold-disabled-property dialog-state, #{:differentially-expressed, :underexpressed}),
          upper-threshold-disabled-prop (threshold-disabled-property dialog-state, #{:differentially-expressed, :overexpressed}),
          histogram-data-prop (jfx/map-entry-property dialog-state, [:histogram-data])
          window (doto (jfx/modal-window "Select threshold(s) for expression z-scores", control-node)
                   (jfx/size-to-scene!)
                   (jfx/prevent-exit!)
                   (jfx/resizable! false)
                   (t/add-avatar-icon))]

      (jfx/listen-to
        (fn [selected-toggle]
          (let [threshold-mode (cond
                                 (= selected-toggle differentially-expressed-radiobutton) :differentially-expressed
                                 (= selected-toggle overexpressed-radiobutton) :overexpressed
                                 (= selected-toggle underexpressed-radiobutton) :underexpressed)]
            (swap! dialog-state assoc :threshold-mode threshold-mode)))
        (jfx/property toggle-group, :selected-toggle))

      ; lower threshold enabled?
      (doseq [^Node node [lower-threshold-label, lower-threshold-textfield, lower-threshold-error-label]]
        (jfx/bind (jfx/property node, :disable) lower-threshold-disabled-prop))

      ; upper threshold enabled?
      (doseq [^Node node [upper-threshold-label, upper-threshold-textfield, upper-threshold-error-label]]
        (jfx/bind (jfx/property node, :disable) upper-threshold-disabled-prop))

      (setup-expression-threshold-textfield dialog-state :lower-threshold, :lower-threshold-error, lower-threshold-disabled-prop, lower-threshold-textfield)
      (setup-expression-threshold-textfield dialog-state :upper-threshold, :upper-threshold-error, upper-threshold-disabled-prop, upper-threshold-textfield)

      (jfx/bind (jfx/property lower-threshold-error-label, :text) (jfx/map-entry-property dialog-state, [:lower-threshold-error]))
      (jfx/bind (jfx/property upper-threshold-error-label, :text) (jfx/map-entry-property dialog-state, [:upper-threshold-error]))

      (jfx/bind histogram-data-prop
        (jfx/functional-property
          (fn [dialog-state-map]
            (calculate-histogram-data
              histogram-bin-count,
              (create-expressed?-fn (extract-expression-thresholds dialog-state-map)),
              values-per-gene))
          (jfx/map-property dialog-state, #{:lower-threshold, :upper-threshold, :threshold-mode})))

      (setup-histogram histogram-chart, histogram-data-prop)

      ; disable apply button, if there is an error
      (jfx/bind (jfx/property apply-button, :disable)
        (jfx/functional-property
          (fn [{:keys [lower-threshold-error, upper-threshold-error]}]
            (or (some? lower-threshold-error) (some? upper-threshold-error)))
          (jfx/map-property dialog-state, #{:lower-threshold-error, :upper-threshold-error})))

      (jfx/property-value! differentially-expressed-radiobutton, :selected, true)

      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (deliver result-promise (extract-expression-thresholds (deref dialog-state)))
          (jfx/close window)))
      (jfx/show window)
      result-promise)))


(defn setup-methylation-threshold-textfield
  [dialog-state, threshold-property-kw, error-property-kw, ^TextField textfield]
  (let [text-prop (jfx/property textfield, :text),
        threshold-prop (jfx/map-entry-property dialog-state, [threshold-property-kw])]
    (jfx/listen-to
      (fn [threshold]
        (jfx/value! text-prop, (str threshold)))
      threshold-prop)
    ; propagate changes to dialog-state
    (jfx/listen-to
      (fn [text]
        (swap! dialog-state
          (fn [state]
            (if (u/is-double? text)
              (assoc state
                threshold-property-kw text
                error-property-kw nil)
              (assoc state
                error-property-kw "must be a floating point number")))))
      text-prop)))


(defn extract-methylation-threshold
  "Extract selected thresholds from dialog state."
  [dialog-state-map]
  (update-in dialog-state-map [:threshold] u/parse-double))


(defn ask-for-methylation-threshold
  [{:keys [values-per-gene] :as methylation-data}]
  (jfx/run-now
    (let [dialog-state (atom {:threshold (u/format-double 0.0),
                              :threshold-error nil,
                              :histogram-data nil}),
          result-promise (promise)
          control (jfx/create-control "avatar/ui/SelectMethylationThresholdDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [histogram-chart,
                  ^TextField
                  threshold-textfield,
                  ^Label
                  threshold-error-label,
                  hypermethylated-radiobutton,
                  hypomethylated-radiobutton,
                  ^Button
                  apply-button]
           :as children} (jfx/control-children control),
          histogram-data-prop (jfx/map-entry-property dialog-state, [:histogram-data])
          window (doto (jfx/modal-window "Select threshold for methylation beta values", control-node)
                   (jfx/size-to-scene!)
                   (jfx/prevent-exit!)
                   (jfx/resizable! false)
                   (t/add-avatar-icon))]

      (setup-methylation-threshold-textfield dialog-state :threshold, :threshold-error, threshold-textfield)

      (jfx/bind (jfx/property threshold-error-label, :text) (jfx/map-entry-property dialog-state, [:threshold-error]))

      (jfx/bind (jfx/map-entry-property dialog-state, [:methylation-mode]),
        (jfx/selected-toggle-property
          {:hypermethylated hypermethylated-radiobutton,
           :hypomethylated hypomethylated-radiobutton}))

      (jfx/bind histogram-data-prop
        (jfx/functional-property
          (fn [dialog-state-map]
            (calculate-histogram-data
              histogram-bin-count,
              (create-methylated?-fn
                (extract-methylation-threshold dialog-state-map)),
              values-per-gene))
          (jfx/map-property dialog-state, #{:threshold, :methylation-mode})))

      (setup-histogram histogram-chart, histogram-data-prop)

      (jfx/handle-event! window, :window-shown
        (fn [_]
          (future
            (Thread/sleep 100)
            (jfx/property-value! threshold-textfield, :text, (u/format-double 1.0)))))

      ; disable apply button, if there is an error
      (jfx/bind (jfx/property apply-button, :disable)
        (jfx/functional-property
          some?
          (jfx/map-entry-property dialog-state, [:threshold-error])))

      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (deliver result-promise (select-keys (deref dialog-state) [:threshold, :methylation-mode]))
          (jfx/close window)))

      (jfx/show window)
      result-promise)))


(defn download-study-list
  [bioportal]
  (try
    (let [studies (bp/study-list bioportal)]
      (mapv #(assoc % ::selected? false) studies))
    (catch Throwable t
      (u/log-error "Study list download failed:\n%s" (with-out-str (st/print-cause-trace t)))
      (jfx/show-error "Study list download failed",
        "The list of studies could not be loaded.",
        (str "It seems that www.cbioportal.org is not reachable.\nError: " (.getMessage t)))
      nil)))


(defn ask-for-alteration-profiles
  [bioportal, {:keys [study-id] :as study-data}]
  (let [available-profiles (progress/with-shown-indeterminate-progress "Scanning data archive for alteration data types ...",
                             (bp/archive-profiles bioportal, study-id)),]
    (jfx/run-now
      (let [dialog-state (atom #{}),
            result (atom nil),
            checkbox-list (->> available-profiles
                            ((juxt :mutation-data, :fusion-data, :expression-data, :methylation-data))
                            (remove nil?)
                            (mapv
                              (fn [{:keys [profile-id, entry-name] :as profile}]
                                (let [checkbox (CheckBox. (format "%s\n(%s)" (gd/alteration-type-str profile-id) entry-name))]
                                  (jfx/listen-to
                                    (fn [selected?]
                                      (swap! dialog-state (if selected? conj disj) profile-id))
                                    (jfx/property checkbox :selected))
                                  checkbox))))
            checkbox-vbox (jfx/add-children (doto (VBox.) (jfx/spacing! 5.0)), checkbox-list)
            checkbox-container (doto (HBox.)
                                 (jfx/padding! {:top 10.0})
                                 (jfx/alignment! :center)
                                 (jfx/add-child checkbox-vbox))
            control (doto (VBox.)
                      (jfx/add-children
                        [(-> (Label. "Select alteration data types to import for study")
                           (jfx/text-font! {:weight :bold}))
                         (-> (Label. (:name study-data))
                           (jfx/text-font!, {:posture :italic}))
                         checkbox-container]))
            window (jfx/dialog "Select alteration data", control,
                     {:modal? true,
                      :width 400
                      :ok-button {:text "Select",
                                  :disable? (jfx/functional-property empty?, (jfx/->property dialog-state)),
                                  :handler (fn [_] (reset! result (deref dialog-state)))}
                      :cancel-button true})]
        (jfx/show-and-wait window)
        (when-let [selected-profile-ids (deref result)]
          (select-keys available-profiles, (conj selected-profile-ids :clinical-samples :clinical-patients)))))))


(defn collect-sample-ids
  "Collect the set of sample ids from the alteration profiles."
  [archive-data-map]
  (reduce-kv
    (fn [id-set, profile-id, raw-data]
      (case profile-id
        (:mutation-data :fusion-data) (into id-set (map :sample) raw-data)
        (:expression-data :methylation-data) (into id-set (:sample-ids raw-data))
        id-set))
    (sorted-set)
    archive-data-map))


(defn collect-genes
  "Collect the set of genes from the alteration profiles."
  [archive-data-map]
  (reduce-kv
    (fn [id-set, profile-id, raw-data]
      (case profile-id
        (:mutation-data :fusion-data) (into id-set (map :gene) raw-data)
        (:expression-data :methylation-data) (into id-set (map (some-fn :hugo-symbol :entrez-gene-id)) (:values-per-gene raw-data))
        id-set))
    (sorted-set)
    archive-data-map))


(defn rename-sample-ids
  [sample-id-renaming-map, alteration-list]
  (mapv
    (fn [alteration-data]
      (update-in alteration-data [:sample] #(get sample-id-renaming-map %)))
    alteration-list))


(defn import-mutation-data
  [sample-id-renaming-map, sample-id->index-map, {:keys [mutation-data, fusion-data] :as archive-data-map}]
  (->> (into (vec mutation-data) fusion-data)
    (rename-sample-ids sample-id-renaming-map)
    (imp/alteration-list->gene-alterations-per-sample sample-id->index-map)))


(defn import-real-valued-alteration-matrix
  [sample-id-renaming-map, sample-id->index-map, altered?-fn, alteration-data]
  (imp/alteration-matrix-gene-alterations-per-sample
    altered?-fn, sample-id->index-map,
    ; rename sample ids
    (update-in alteration-data [:sample-ids]
      (fn [sample-ids]
        (mapv #(get sample-id-renaming-map %) sample-ids)))))


(defn combine-to-gene-sample-data
  [alteration-data-map, study-name, sample-ids, genes]
  (let [sample-id-vec (vec sample-ids),
        sample-count (count sample-id-vec),
        sample-group-map (zipmap (range sample-count) (repeat study-name)),
        gene-list (persistent!
                    (reduce
                      (fn [gene-list, gene-name]
                        (let [alterations-map (persistent!
                                                (reduce-kv
                                                  (fn [result-map, alteration-type, gene->alterations-map]
                                                    (if-let [alteration-data (get gene->alterations-map gene-name)]
                                                      (assoc! result-map alteration-type alteration-data)
                                                      ; missing gene, mark all sample values as missing
                                                      (assoc! result-map
                                                        alteration-type
                                                        {:alterations (im/dense-int-set),
                                                         :missing-values (c/populated-dense-int-set sample-count)})))
                                                  (transient {})
                                                  alteration-data-map))]
                          (conj! gene-list
                            {:gene gene-name,
                             :group nil,
                             :order-fixed? false,
                             :alteration-data alterations-map})))
                      (transient [])
                      (sort genes)))]
    {:gene-list gene-list
     :sample-count sample-count
     :sample-id-vec sample-id-vec,
     :sample-group-map sample-group-map,
     :alteration-type-set (into (sorted-set) (keys alteration-data-map)),
     :sample-permutation {:sample->column (vec (range sample-count)),
                          :column->sample (vec (range sample-count))}}))


(defn import-alteration-data
  [bioportal, {:keys [study-id, name]}, selected-alteration-profiles]
  (let [archive-data-map (bp/import-archive bioportal, study-id, selected-alteration-profiles),
        original-sample-ids (collect-sample-ids archive-data-map),
        ; sample renaming map such that they correspond to the sample ids in clinical data
        sample-id-renaming-map (imp/sample-id-renaming (:clinical-samples archive-data-map), original-sample-ids)
        unified-sample-ids (->> sample-id-renaming-map vals (into (sorted-set))),
        ; assign sample id to index
        sample-id->index-map (zipmap unified-sample-ids (range)),
        genes (collect-genes archive-data-map),
        expression-thresholds (when (contains? archive-data-map :expression-data)
                                (deref (ask-for-expression-thresholds (:expression-data archive-data-map),)))
        methylation-thresholds (when (contains? archive-data-map :methylation-data)
                                 (deref (ask-for-methylation-threshold (:methylation-data archive-data-map),)))]
    (progress/with-shown-indeterminate-progress "Importing Alteration Data ..."
      (cond-> {}
        (or (contains? archive-data-map :mutation-data) (contains? archive-data-map :fusion-data))
        (assoc :mutation-data (import-mutation-data sample-id-renaming-map, sample-id->index-map, archive-data-map))

        (and (contains? archive-data-map :expression-data) expression-thresholds)
        (assoc
          :expression-data
          (let [expressed?-fn (create-expressed?-fn expression-thresholds)]
            (import-real-valued-alteration-matrix
              sample-id-renaming-map, sample-id->index-map, expressed?-fn, (:expression-data archive-data-map))))

        (and (contains? archive-data-map :methylation-data) methylation-thresholds)
        (assoc
          :methylation-data
          (let [methylated?-fn (create-methylated?-fn (extract-methylation-threshold methylation-thresholds))]
            (import-real-valued-alteration-matrix
              sample-id-renaming-map, sample-id->index-map, methylated?-fn, (:methylation-data archive-data-map))))

        ; combine alteration data
        true
        (combine-to-gene-sample-data name, unified-sample-ids, genes)

        ; add clinical data
        true
        (->> (imp/add-clinical-data name, archive-data-map))))))





(defn show-dialog
  [{:keys [bioportal] :as ui}, complete-fn]
  (jfx/run-later
    (let [study-list-atom (atom nil)
          study-list-future (u/safe-future (reset! study-list-atom (download-study-list bioportal)))
          dialog-state (atom {:selected-study-id nil})
          import-view (jfx/create-control "avatar/ui/BioportalImportDialog.fxml"),
          import-view-node (jfx/control-node import-view)
          {:keys [^TableView study-table,
                  ^TextField search-textfield,
                  found-study-count-label,
                  ^Button
                  import-button,
                  cancel-button] :as child-nodes} (jfx/control-children import-view),
          window (-> (jfx/modal-window "Import data from cbioportal", import-view-node, 1200, 800)
                   (jfx/user-data! dialog-state)
                   (t/add-avatar-icon))
          update-found-studies (fn [table-rows] (jfx/run-now (jfx/label-text! found-study-count-label (format "Found studies: %d" (count table-rows))))),
          import (fn import []
                   (u/safe-future
                     (let [{:keys [selected-study-id]} (deref dialog-state),
                           study-list (deref study-list-atom),
                           {:keys [study-id] :as study-data} (u/some-value #(= (:study-id %) selected-study-id) study-list),
                           download-result (try
                                             (bp/download-study-data
                                               bioportal,
                                               study-id)
                                             (catch Throwable t
                                               (u/log-exception
                                                 (format "Exception during archive download of study \"%s\"" study-id),
                                                 t)
                                               :bioportal/DOWNLOAD-ERROR))]
                       (when-not (= download-result :bioportal/DOWNLOAD-ERROR)
                         (when-let [selected-alteration-profiles (ask-for-alteration-profiles bioportal, study-data)]
                           (jfx/close window)
                           (when-let [alteration-data (try
                                                        (import-alteration-data bioportal, study-data, selected-alteration-profiles)
                                                        (catch Throwable t
                                                          (u/log-exception "Bioportal Import: Converting alteration data to gene-sample-data failed!", t)
                                                          (jfx/show-error
                                                            "Bioportal Import failed",
                                                            "Import of downloaded alteration data failed.",
                                                            nil)
                                                          nil))]
                             (complete-fn alteration-data)))))))]

      ; setup study table
      (jfx/setup-table-view! study-table, [:study-id, :name, :description], study-list-atom,
        :filter-text-property (jfx/property search-textfield, :text),
        :sort? true)



      ; setup label showing the number of found studies
      (update-found-studies (jfx/property-value study-table, :items))
      (jfx/invalidation-listener! (jfx/property-value study-table, :items) update-found-studies)

      (jfx/bind (jfx/map-entry-property dialog-state, [:selected-study-id])
        (jfx/functional-property
          (fn [selected-row]
            (when selected-row
              (-> selected-row (get-in [:column-properties, :study-id]) jfx/value)))
          (jfx-select/selected-item-property study-table)))

      (jfx/bind (jfx/property import-button, :disable)
        (jfx/functional-property nil?,
          (jfx/map-entry-property dialog-state, [:selected-study-id])))

      (jfx/handle-event! cancel-button :action
        (fn [_]
          (jfx/close window)))
      (jfx/handle-event! import-button :action
        (fn [_]
          (import)))
      (jfx/handle-event! study-table, :mouse-clicked,
        (fn [e]
          (when (jfx/double-click? e)
            (import))))
      (jfx/show window))))
