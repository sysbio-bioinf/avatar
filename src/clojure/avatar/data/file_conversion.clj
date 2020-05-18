; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.file-conversion
  (:require
    [frost.quick-freeze :as qf]
    [avatar.version :as v]
    [clojure.string :as str]
    [clj-jfx.core :as jfx]
    [avatar.util :as u]
    [avatar.data.serializers :as ds]
    [avatar.data.schema :as schema]
    [avatar.ui.dialogs.progress-view :as progress]
    [avatar.data.import :as imp]
    [avatar.ui.tools :as t]
    [clojure.set :as set]
    [clojure.java.io :as io])
  (:import
    (java.io File)))



(defn combine-conversion-fns
  [conversion-fn-list]
  (fn [gene-sample-data]
    (reduce
      (fn [gene-sample-data, f] (f gene-sample-data))
      gene-sample-data
      conversion-fn-list)))


(defn convert-gene-sample-data
  "Update the plot data to the new format via the given functions."
  [conversion-fn-list, file-data]
  (let [convert (combine-conversion-fns conversion-fn-list)]
    (-> file-data
      (update-in [:data, :history] #(mapv convert %))
      (update-in [:snapshots]
        (fn [snapshots]
          (mapv #(update-in % [:data] convert) snapshots)))
      (update-in [:latest-pareto-front]
        (fn [latest-pareto-front]
          (when latest-pareto-front
            ; update :plot-data in pareto front solutions
            (update-in latest-pareto-front [:pareto-front]
              (fn [pareto-front]
                (mapv
                  #(update-in % [:plot-data] convert)
                  pareto-front)))))))))


(defn add-clinical-attributes
  "Adds the set of clinical attribute when a non-empty clinical data map is present."
  [{:keys [clinical-data-map, clinical-attributes] :as gene-sample-data}]
  (cond-> gene-sample-data
    (and (seq clinical-data-map) (empty? clinical-attributes))
    (assoc :clinical-attributes (imp/clinical-attributes clinical-data-map))))


(defn multiple-alteration-types
  [alteration-type, {:keys [mutations-per-patient, missing-values-per-patient] :as gene-data}]
  (-> gene-data
    (dissoc
      :mutations-per-patient,
      :missing-values-per-patient,
      :avatar.algorithms.mo-select/coverage,
      :avatar.algorithms.mo-select/index)
    (assoc
      :alteration-data
      {alteration-type {:alterations mutations-per-patient, :missing-values missing-values-per-patient}})))


(defn gene-sample-data-029->030-conversion
  [alteration-type,
   {:keys [patient-count,
           patient-group-map,
           patient-id-vec,
           patient-permutation,
           gene-list]
    :as gene-sample-data}]
  (-> gene-sample-data
    (dissoc
      :patient-count
      :patient-group-map
      :patient-id-vec
      :patient-permutation)
    (assoc
      :alteration-type-set #{alteration-type}
      :sample-count patient-count
      :sample-group-map patient-group-map
      :sample-id-vec patient-id-vec,
      :sample-permutation (let [{:keys [patient->column, column->patient]} patient-permutation]
                            {:sample->column patient->column,
                             :column->sample column->patient})
      :gene-list (mapv (partial multiple-alteration-types alteration-type), gene-list))))





(defn convert-latest-pareto-front-029->030
  [alteration-type, file-data]
  (update-in file-data [:latest-pareto-front]
    (fn [{:keys [patient-group, second-objective] :as latest-pareto-front}]
      ; update :latest-pareto-front if exists
      (when latest-pareto-front
        (-> latest-pareto-front
          (dissoc
            :patient-group
            :second-objective)
          (update-in [:parameters] assoc
            :sample-group patient-group,
            :alteration-type alteration-type,
            :second-objective second-objective,
            :algorithm :UNKNOWN,
            :exclude-fixed-rows? true,
            :mutation-probability Double/NaN,
            :crossover-probability Double/NaN,
            :selection-probability Double/NaN,
            :iteration-count -1,
            :tournament-size -1,
            :population-size -1,)
          (update-in [:pareto-front]
            (fn [pareto-front]
              ; update :plot-data in the solutions of :pareto-front
              (mapv
                (fn [pareto-front-solution]
                  (let [pareto-front-solution (update-in pareto-front-solution
                                                [:selection]
                                                (partial mapv (partial multiple-alteration-types alteration-type)))]
                    (cond-> pareto-front-solution
                      ; previous conversion to 0.2.9 did not add :clinical-attributes to :plot-data in :latest-pareto-front solutions
                      ; in that case do that now
                      (not (contains? (:plot-data pareto-front-solution) :clinical-attributes))
                      (update-in [:plot-data] add-clinical-attributes))))
                pareto-front))))))))


(defn convert-029->030
  [file-data]
  (let [alteration-type (t/ask-for-alteration-type #{:mutation-data, :expression-data, :methylation-data}
                          "Upgrade from 0.2.9 to 0.3.0"
                          "Which type of alterations does the file contain?")]
    (-> file-data
      (update-in [:plot-style]
        set/rename-keys {:patient-group-font :sample-group-font,
                         :patient-groups-colored? :sample-groups-colored?,
                         :patient-group->color :sample-group->color,
                         :mutation-color :alteration-color,
                         :mutation-ratio-digits :alteration-ratio-digits,
                         :mutation-ratio-font :alteration-ratio-font,
                         :mutation-cell-width :alteration-cell-width})
      (assoc-in [:plot-style, :shown-alteration-type] alteration-type)
      (update-in [:snapshots]
        (fn [snapshots]
          (mapv
            #(assoc % :shown-alteration-type alteration-type)
            snapshots)))
      (->>
        (convert-gene-sample-data [(partial gene-sample-data-029->030-conversion alteration-type)])
        (convert-latest-pareto-front-029->030 alteration-type)))))


(defn add-snapshot-ids
  [snapshots]
  (mapv
    (fn [snap, id]
      (assoc snap :id id))
    snapshots
    (range)))


(defn convert-pareto-front-data
  [{:keys [latest-pareto-front, saved-pareto-fronts] :as file-data}]
  (-> file-data
    (dissoc :latest-pareto-front, :saved-pareto-fronts)
    (assoc
      :pareto-front-data
      {:single {:saved saved-pareto-fronts
                :latest latest-pareto-front}
       :batch []})))


(defn convert-030->034
  [file-data]
  (-> file-data
    (update-in [:snapshots] add-snapshot-ids)
    convert-pareto-front-data))


(defn convert-pareto-fronts
  [f, file-data]
  (-> file-data
    (update-in [:pareto-front-data, :single, :latest] #(some-> % f))
    (update-in [:pareto-front-data, :single, :saved] (partial mapv #(update-in % [:data] f)))
    (update-in [:pareto-front-data, :batch]
      (fn [batch-list]
        (mapv
          (fn [batch]
            (update-in batch [:pareto-front-list] (partial mapv #(update-in % [:data] f))))
          batch-list)))))


(defn add-minimum-alteration-ratio
  [{:keys [parameters] :as pareto-front}]
  (if (contains? parameters :minimum-alteration-ratio)
    pareto-front
    (assoc-in pareto-front [:parameters, :minimum-alteration-ratio] 0.0)))


(def file-format-updates
  (sorted-map
    ; conversion from 0.2.8 to 0.2.9
    [0 2 9] (partial convert-gene-sample-data [add-clinical-attributes])
    [0 3 0] convert-029->030
    [0 3 4] convert-030->034
    [0 3 6] (partial convert-pareto-fronts add-minimum-alteration-ratio)))


(defn backup-file
  ^File [file, version]
  (u/amend-filename file, (str ".backup-" (v/version-str version))))


(defn maybe-convert-file
  [file]
  (let [{file-version :version} (ds/file-info file)]
    (when (v/previous-version? file-version)
      (let [update-fn-map (reduce-kv
                            (fn [update-fn-map, version, update-fn]
                              (cond-> update-fn-map
                                (v/previous-version? file-version version) (assoc version update-fn)))
                            (sorted-map)
                            file-format-updates)]
        (when (seq update-fn-map)
          (u/log-debug "Conversion of file \"%s\" required: %s -> %s"
            file,
            (v/version-str file-version)
            (->> update-fn-map keys (mapv v/version-str) (str/join " -> ")))
          (if (jfx/ask-confirmation
                "Open file",
                (format "Convert the file \"%s\" to the new file format?" file),
                (format "The file has been saved with version %s and needs to be converted to version %s.\nOtherwise the file cannot be opened."
                  (v/version-str file-version),
                  (v/version-str (->> update-fn-map keys last))))
            (progress/with-shown-indeterminate-progress (format "Converting file %s to new file format ..." (u/file-name file))
              (u/log-debug "File conversion confirmed.")
              (let [backup-file (backup-file file, file-version)]
                (u/log-debug "Saving backup to %s" (.getPath backup-file))
                (u/file-copy file, backup-file))
              (let [file-data (try
                                (ds/load-from-file file)
                                (catch Throwable t
                                  (let [msg (format "Failed to read data of file \"%s\"" (u/file-absolute-path file))]
                                    (u/log-exception msg, t)
                                    (throw
                                      (ex-info
                                        msg
                                        {:scope :convert-file, :action :read-data, :file file}
                                        t))))),
                    converted-file-data (try
                                          (reduce-kv
                                            (fn [file-data, version, update-fn]
                                              (u/log-debug "Converting to %s" (v/version-str version))
                                              (update-fn file-data))
                                            file-data
                                            update-fn-map)
                                          (catch Throwable t
                                            (let [msg (format "Failed to convert data of file \"%s\" from version %s to version %s."
                                                        (u/file-absolute-path file), (v/version-str file-version), (v/version-str))]
                                              (u/log-exception msg, t)
                                              (throw
                                                (ex-info
                                                  msg
                                                  {:scope :convert-file, :action :convert-data, :file file}
                                                  t)))))]
                (if-let [error-map (schema/validate converted-file-data)]
                  ; format invalid after conversion
                  (let [msg (format "Failed conversion of file \"%s\" from version %s to version %s: the data format of the conversion result is invalid."
                              (u/file-absolute-path file), (v/version-str file-version), (v/version-str))]
                    (u/log-error msg)
                    (throw
                      (ex-info
                        msg
                        {:scope :convert-file, :action :format-validation, :file file, :error-map error-map})))
                  ; format is valid: store data to file
                  (try
                    (ds/save-to-file file, converted-file-data)
                    (u/log-debug "Conversion of file \"%s\" completed." (u/file-absolute-path file))
                    (catch Throwable t
                      (u/log-exception "Failed to write converted file data!", t)
                      (throw
                        (ex-info
                          (format "Failed to write converted data to file \"%s\"!" (u/file-absolute-path file))
                          {:scope :convert-file, :action :write-converted-data, :file file}
                          t)))))))
            (do
              (u/log-debug "File conversion declined.")
              :file-not-converted)))))))
