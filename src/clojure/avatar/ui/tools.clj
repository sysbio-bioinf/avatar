; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.tools
  (:require
    [clojure.string :as str]
    [clj-jfx.core :as jfx]
    [avatar.data.gene-data :as gd]
    [clj-jfx.colors :as col]
    [avatar.util :as u]
    [clojure.java.io :as io])
  (:import
    (javafx.scene.control ComboBox CheckBox Label)
    (javafx.scene.layout HBox VBox)
    (javafx.stage Stage)
    (javafx.scene.paint Color)
    (javafx.scene.image Image)))



(defn setup-alteration-type-combobox
  [combobox, alteration-types, & {:keys [alteration-type-change, selected-alteration-type]}]
  (let [alteration-types (vec (gd/sort-alteration-types alteration-types))
        alteration-type-prop (jfx/value->keyword-property
                               (zipmap alteration-types (range)),
                               (jfx/selected-index-property combobox))]
    (jfx/setup-combobox combobox, (mapv gd/alteration-type-str alteration-types))
    (if selected-alteration-type
      (jfx/select-item! combobox, (gd/alteration-type-str selected-alteration-type))
      (jfx/select-first! combobox))
    (when alteration-type-change
      (jfx/listen-to alteration-type-change, alteration-type-prop))
    alteration-type-prop))


(defn alteration-type?
  "Is the given value one of the alteration types?"
  [x]
  (contains? gd/alteration-type-set x))


(defn setup-alteration-type-checkbox-control
  ([alteration-types]
   (setup-alteration-type-checkbox-control alteration-types, nil))
  ([alteration-types, selected-alteration-type]
   (let [control-state (atom (cond-> #{} selected-alteration-type (conj selected-alteration-type)))
         checkbox-list (->> alteration-types
                         gd/sort-alteration-types
                         (mapv
                           (fn [alteration-type]
                             (let [checkbox (CheckBox. (gd/alteration-type-str alteration-type))]
                               (when (and selected-alteration-type (= alteration-type selected-alteration-type))
                                 (jfx/property-value! checkbox, :selected, true))
                               (jfx/listen-to
                                 (fn [selected?]
                                   (swap! control-state (if selected? conj disj) alteration-type))
                                 (jfx/property checkbox :selected))
                               checkbox))))
         checkbox-vbox (jfx/add-children (doto (VBox.) (jfx/spacing! 5.0)), checkbox-list)
         checkbox-container (doto (HBox.)
                              (jfx/padding! {:top 10.0, :bottom 10.0})
                              (jfx/alignment! :center)
                              (jfx/add-child checkbox-vbox))]
     {:checkbox-control checkbox-container, :selection-state control-state})))


(defn alteration-selection-for-deletion-dialog
  "Shows a GUI dialog to select alteration types that will be used for deletion of genes or samples (specified data-type).
  A set of the selected alteration types are returned."
  [{:keys [alteration-type-set, gene-list, sample-count] :as gene-sample-data}, title, data-type]
  (assert (contains? #{:sample, :gene} data-type) "data-type must be either :sample or :gene")
  (let [result (promise)]
    (jfx/run-now
      (let [query-label (Label.
                          (format "Delete %ss without alterations in the following alteration types:" (name data-type)))
            {:keys [checkbox-control, selection-state]} (setup-alteration-type-checkbox-control alteration-type-set),
            selection-prop (jfx/->property selection-state),
            find-fn (case data-type
                      :gene gd/find-genes-without-alterations
                      :sample gd/find-samples-without-alterations)
            matching-entities-count-prop (jfx/functional-property
                                           (fn [selection]
                                             (when (seq selection)
                                               (count (find-fn selection, gene-sample-data))))
                                           selection-prop),
            info-label (doto (Label.)
                         (->
                           (jfx/property :text)
                           (jfx/bind
                             (jfx/functional-property
                               (fn [matching-entities-count]
                                 (let [matching-entities-count (or matching-entities-count 0)]
                                   (format "%2$d %1$ss will be deleted and %3$d %1$ss will be kept."
                                     (name data-type),
                                     matching-entities-count
                                     (case data-type
                                       :gene (- (count gene-list) matching-entities-count)
                                       :sample (- sample-count matching-entities-count)))))
                               matching-entities-count-prop))))
            control (doto (VBox.)
                      (jfx/add-children
                        [query-label
                         checkbox-control
                         info-label]))
            dialog (jfx/dialog title, control,
                     {:modal? true,
                      :ok-button {:text "Delete",
                                  :disable? (jfx/functional-property empty?, selection-prop),
                                  :handler (fn [_]
                                             (deliver result (deref selection-state)))}
                      :cancel-button {:handler (fn [_]
                                                 (deliver result nil))}})]
        (jfx/show-and-wait dialog)))
    (deref result)))


(defn ask-for-alteration-type
  "Asks the user to specify an alteration type from the given set of alteration types.
  A custom title and message need to be specified."
  [alteration-types, title, message]
  (let [result (promise)]
    (jfx/run-now
      (let [alteration-type-combobox (ComboBox.)
            alteration-type-prop (setup-alteration-type-combobox alteration-type-combobox, alteration-types)
            control (doto (VBox.)
                      (jfx/add-children
                        (vector
                          (Label. ^String message),
                          (doto (HBox.)
                            (jfx/padding! {:top 10.0})
                            (jfx/alignment! :center)
                            (jfx/add-child alteration-type-combobox)))))
            dialog (jfx/dialog title, control,
                     {:modal? true,
                      :prevent-exit? true,
                      :ok-button {:text "Select",
                                  :handler (fn [_]
                                             (deliver result (jfx/value alteration-type-prop)))},
                      :close-handler (fn [_] (deliver result nil))})]
        (jfx/show-and-wait dialog)))
    (deref result)))


(defn primary-stage
  ^Stage [ui]
  (:primary-stage (deref (:controls-atom ui))))


(defonce letters (mapv #(char (+ (long \A) %)) (range 26)))

(defn excel-letters
  [^long column-index]
  (loop [value column-index, s (StringBuilder.)]
    (let [letter (nth letters (mod value 26))
          s (doto s (.append letter))
          new-value (unchecked-dec (quot value 26))]
      (if (>= value 26)
        (recur new-value, s)
        (str/reverse (str s))))))


(defn color-combination
  [^Color color-1, ^double ratio, ^Color color-2]
  (assert (<= 0.0 ratio 1.0) "ratio must be between 0.0 and 1.0")
  (let [color-1 (col/to-color color-1),
        color-2 (col/to-color color-2)]
    (Color.
      (+ (* ratio (.getRed color-1)) (* (- 1 ratio) (.getRed color-2)))
      (+ (* ratio (.getGreen color-1)) (* (- 1 ratio) (.getGreen color-2)))
      (+ (* ratio (.getBlue color-1)) (* (- 1 ratio) (.getBlue color-2)))
      1.0)))


(defn ask-for-text-export-file
  [window, data-type]
  (jfx/choose-file window, (format "Export %s data" data-type), :save,
    {:file-types [["Tab separated file (*.tsv)" ["*.tsv"]],
                  ["Semicolon separated file (*.ssv)" ["*.ssv"]],
                  ["Comma separated file (*.csv)" ["*.csv"]],]}))


(defn watch-property
  ([prop]
   (watch-property nil, prop))
  ([context, prop]
   (jfx/change-listener! prop
     (fn [_, old-value, new-value]
       (when-not (= old-value new-value)
         (println)
         (when context
           (println context))
         (u/print-differences old-value, new-value))))))


(defn resource-image
  [path]
  (Image. (io/input-stream (io/resource path))))


(defn add-avatar-icon
   [^Stage stage]
  (doto stage
    (->
      .getIcons
      (.add (resource-image "avatar/ui/avatar-256.png")))))