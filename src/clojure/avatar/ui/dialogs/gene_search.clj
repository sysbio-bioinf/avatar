; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.gene-search
  (:require
    [clj-jfx.core :as jfx]
    [avatar.util :as u]
    [avatar.data.gene-symbol-synonyms :as gss]
    [avatar.algorithms.edit-distance :as ed]
    [clojure.string :as str]
    [clj-jfx.selection :as jfx-select]
    [avatar.ui.tools :as t])
  (:import
    (javafx.scene.control TextField CheckBox Button Label)))





(defn maybe-download-synonyms
  [synonym-service, dialog-state]
  (let [{:keys [synonym-data]} (deref dialog-state)]
    (when-not (seq synonym-data)
      (u/safe-future
        (when-let [synonym-data (gss/get-gene-symbol-synonym-data synonym-service)]
          (swap! dialog-state assoc :synonym-data synonym-data))))))


(defn compare-similarity-strs
  [similarity-1, similarity-2]
  (cond
    (= similarity-1 similarity-2)
    0

    (= similarity-1 "exact match")
    1

    (= similarity-2 "exact match")
    -1

    (= similarity-1 "synonym")
    1

    (= similarity-2 "synonym")
    -1

    :both-number-strs
    (compare similarity-1 similarity-2)))


(defn compare-matches
  [match-1, match-2]
  (let [similarity-1 (:similarity match-1),
        similarity-2 (:similarity match-2)]
    (cond
      (= similarity-1 similarity-2)
      0

      (= similarity-1 :exact-match)
      1

      (= similarity-2 :exact-match)
      -1

      (= similarity-1 :synonym)
      1

      (= similarity-2 :synonym)
      -1

      :both-numbers
      (compare similarity-1 similarity-2))))


(defn find-matches
  [gene-name-list, limit, search-text, use-synonyms?, {:keys [symbol->synonyms, synonym->symbols] :as synonym-data}]
  (let [search-text (str/trim search-text),
        synonyms (when use-synonyms?
                   (-> #{}
                     (into (get symbol->synonyms search-text))
                     (into (get synonym->symbols search-text)))),
        gene-names-with-similarity (mapv
                                     (fn [gene-name]
                                       {:gene-name gene-name
                                        :similarity
                                        (cond
                                          (= search-text gene-name)
                                          :exact-match

                                          (some #(= % gene-name) synonyms)
                                          :synonym

                                          :else
                                          (ed/similarity search-text, gene-name))})
                                     gene-name-list)]
    (->> gene-names-with-similarity
      (remove (comp (fn [x] (and (number? x) (zero? x))) :similarity))
      (sort (comp - compare-matches))
      (take limit)
      (vec))))



(defn move-on
  [{:keys [search-gene-names, position] :as state}]
  (let [new-position (inc position)]
    (assoc state
      :position new-position
      :search-text (if (< new-position (count search-gene-names))
                     (nth search-gene-names new-position)
                     ""))))


(defn on-multiple-genes-select
  [dialog-state]
  (swap! dialog-state
    (fn [{:keys [selected-row, selected-gene-names] :as state}]
      (assoc (move-on state)
        :selected-gene-names (conj selected-gene-names (:gene-name selected-row))))))


(defn on-multiple-genes-skip
  [dialog-state]
  (swap! dialog-state
    (fn [{:keys [selected-gene-names] :as state}]
      (assoc (move-on state)
        ; for easier debugging, add nil to signal skipping
        :selected-gene-names (conj selected-gene-names nil)))))


(defn select-skip-handler
  [complete-fn, window, dialog-state, handler-fn]
  (fn [& _]
    (let [{:keys [position, search-gene-names, selected-gene-names]} (handler-fn dialog-state)]
      (when (>= position (count search-gene-names))
        (complete-fn (u/removev nil? selected-gene-names))
        (jfx/close window)))))


(defn show-dialog
  [{:keys [synonym-service] :as ui}, available-gene-name-list, search-type, search-gene-name-list, complete-fn]
  (jfx/run-now
    (let [dialog-state (atom
                         (case search-type
                           :single-gene {}
                           :multiple-genes (let [search-gene-names (vec search-gene-name-list)]
                                             {:search-gene-names search-gene-names,
                                              :position 0,
                                              :search-text nil,
                                              :selected-gene-names []}))),
          control (jfx/create-control "avatar/ui/GeneSearchDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [^TextField search-textfield,
                  ^CheckBox
                  synonyms-checkbox,
                  matches-tableview,
                  ^Label
                  position-label,
                  ^Button
                  select-button,
                  skip-button,
                  cancel-button,]
           :as children} (jfx/control-children control),
          window (doto (jfx/modal-window
                         (case search-type :single-gene "Find Gene" :multiple-genes "Find Genes"),
                         control-node)
                   (t/add-avatar-icon)),
          search-text-prop (jfx/map-entry-property dialog-state, [:search-text]),
          matches-prop (jfx/map-entry-property dialog-state, [:matches]),
          selected-row-prop (jfx/map-entry-property dialog-state, [:selected-row]),
          table-data (atom [])]

      (jfx/bind-bidirectional search-text-prop,
        (jfx/property search-textfield, :text))

      (jfx/bind-bidirectional (jfx/map-entry-property dialog-state, [:use-synonyms?])
        (jfx/property synonyms-checkbox, :selected))

      (jfx/listen-to
        (fn [use-synonyms?]
          (when use-synonyms?
            (maybe-download-synonyms synonym-service, dialog-state)))
        (jfx/map-entry-property dialog-state, [:use-synonyms?]))


      (jfx/setup-table-view! matches-tableview, [:gene-name, :similarity], table-data,
        :sort? true,
        :column-cell-value-fns {:similarity (fn [similarity]
                                              (if (keyword? similarity)
                                                (-> similarity name (str/replace #"-" " "))
                                                (format "%.3f" similarity)))})

      (jfx/column-comparator! matches-tableview, 1, compare-similarity-strs)


      (jfx/bind matches-prop
        (jfx/functional-property
          (partial find-matches available-gene-name-list, 100)
          search-text-prop,
          (jfx/map-entry-property dialog-state, [:use-synonyms?])
          (jfx/map-entry-property dialog-state, [:synonym-data])))


      (jfx/listen-to
        (fn [matches]
          (reset! table-data matches)
          (jfx/sort-by-column! matches-tableview, 1)
          (jfx-select/clear-selection matches-tableview))
        matches-prop)


      (jfx/bind selected-row-prop
        (jfx/functional-property
          first
          (jfx/selected-rows-property matches-tableview)))


      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      (case search-type
        :single-gene (let [select-match (fn [& _]
                                          (complete-fn (:gene-name (jfx/value selected-row-prop)))
                                          (jfx/close window))]
                       (jfx/remove-children (jfx/parent skip-button), skip-button)
                       (jfx/remove-children (jfx/parent position-label), position-label)
                       (jfx/bind (jfx/property select-button, :disable)
                         (jfx/functional-property
                           nil?
                           selected-row-prop))

                       (jfx/handle-event! select-button, :action, select-match)

                       (jfx/handle-event! matches-tableview, :mouse-clicked,
                         (fn [e]
                           (when (jfx/double-click? e)
                             (select-match)))))
        :multiple-genes (let [select-fn (select-skip-handler complete-fn, window, dialog-state, on-multiple-genes-select)]
                          (jfx/editable! search-textfield, false)

                          (jfx/property-value! search-textfield, :text, (first search-gene-name-list))

                          ; position label

                          (jfx/bind (jfx/property position-label, :text)
                            (jfx/functional-property
                              (fn [position, search-gene-names]
                                (format "%d/%d" (inc position) (count search-gene-names)))
                              (jfx/map-entry-property dialog-state, [:position])
                              (jfx/map-entry-property dialog-state, [:search-gene-names])))

                          ; select button

                          (jfx/bind (jfx/property select-button, :disable)
                            (jfx/functional-property
                              nil?
                              selected-row-prop))

                          (jfx/handle-event! select-button, :action select-fn)

                          (jfx/handle-event! matches-tableview, :mouse-clicked,
                            (fn [e]
                              (when (jfx/double-click? e)
                                (select-fn))))


                          ; skip button

                          (jfx/handle-event! skip-button, :action, (select-skip-handler complete-fn, window, dialog-state, on-multiple-genes-skip))))

      (jfx/show window))))