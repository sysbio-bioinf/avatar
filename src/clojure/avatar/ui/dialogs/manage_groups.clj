; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.manage-groups
  (:require [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [clj-jfx.selection :as jfx-select]
            [clojure.string :as str]
            [avatar.ui.tools :as t])
  (:import (javafx.scene.control Button Label ComboBox)
           (de.jensd.fx.glyphs.materialdesignicons MaterialDesignIconView)))




(defn ask-for-entity-name
  [entity-count-fn, selected-entity-map]
  (let [result (promise)]
    (jfx/run-now
      (let [control (jfx/create-control "avatar/ui/GroupEditor.fxml"),
            control-node (jfx/control-node control),
            {:keys [^Label description-label,
                    ^ComboBox
                    group-combobox,
                    change-button,
                    cancel-button]
             :as children} (jfx/control-children control),
            window (doto (jfx/modal-window "Rename group", control-node)
                     (t/add-avatar-icon))]

        (jfx/property-value! description-label, :text, "Rename group to")
        (jfx/setup-combobox-first-selected! group-combobox, (->> selected-entity-map (sort-by (comp entity-count-fn second) >) (mapv first)))


        (jfx/prevent-exit! window)

        (jfx/handle-event! change-button, :action,
          (fn [_]
            (deliver result (.getValue group-combobox))
            (jfx/close window)))


        (jfx/handle-event! cancel-button, :action,
          (fn [_]
            (deliver result nil)
            (jfx/close window)))

        (jfx/show window)
        ; select all text of combobox
        (doto (.getEditor group-combobox)
          (jfx/request-focus)
          (jfx/select-all-text))))
    (deref result)))



(defn update-renaming-map
  [renaming-map, change-map]
  (persistent!
    (reduce-kv
      (fn [result-map, old-group, new-group]
        (assoc! result-map
          old-group
          ; either the modified new-group or the current new-group
          (get change-map new-group new-group)))
      (transient {})
      renaming-map)))



(defn rename-group
  [entity-merge-fn, dialog-data, selected-group-name]
  (let [new-group-name (jfx/text-input "Rename group",
                         (format "Rename group \"%s\"" selected-group-name),
                         "Enter new group name")]
    (when-not (str/blank? new-group-name)
      (swap! dialog-data
        #(-> %
           (update-in [:entity-map]
             (fn [entity->size-map]
               (-> entity->size-map
                 (dissoc selected-group-name)
                 (assoc
                   new-group-name
                   (cond-> (get entity->size-map selected-group-name)
                     ; when selected group is renamed to existing group
                     (contains? entity->size-map new-group-name)
                     ; add group-size of existing group
                     (entity-merge-fn (get entity->size-map new-group-name)))))))
           (update-in [:renaming-map] update-renaming-map {selected-group-name new-group-name}))))))


(defn merge-groups
  [entity-count-fn, entity-merge-fn, dialog-data, selected-groups]
  (u/safe-future
    (let [{:keys [entity-map]} (deref dialog-data)]
      (when-let [new-group-name (ask-for-entity-name entity-count-fn, (select-keys entity-map, selected-groups))]
        (swap! dialog-data
          #(-> %
             (update-in [:entity-map]
               (fn [entity->size-map]
                 (-> (apply dissoc entity->size-map selected-groups)
                   (assoc
                     new-group-name
                     (transduce
                       (map (partial get entity->size-map))
                       entity-merge-fn
                       selected-groups)))))
             (update-in [:renaming-map] update-renaming-map (zipmap selected-groups (repeat new-group-name)))))))))


(defn delete-groups
  [dialog-data, selected-groups]
  (swap! dialog-data
    #(-> %
       (update-in [:entity-map]
         (fn [entity->size-map]
           (apply dissoc entity->size-map selected-groups)))
       (update-in [:renaming-map] update-renaming-map (zipmap selected-groups (repeat nil))))))


(defn create-table-data
  [entity-count-fn, entity-map]
  (->> entity-map
    (mapv
      (fn [[entity, entity-data]]
        {:entity entity,
         :count (entity-count-fn entity-data)}))
    (sort-by (comp str/lower-case :entity))
    (vec)))


(defn table-edit-dialog
  [{:keys [additional-button, merge-predicate, entity-map, entity-count-fn, entity-merge-fn, window-title, entity-column-name, count-column-name]}, complete-fn]
  (jfx/run-now
    (let [dialog-data (atom {:renaming-map (zipmap (keys entity-map) (keys entity-map)),
                             :entity-map entity-map}),
          table-data (atom (create-table-data entity-count-fn, entity-map))
          control (jfx/create-control "avatar/ui/ManageGroupsDialog.fxml"),
          control-node (jfx/control-node control),
          {:keys [group-tableview,
                  ^Button
                  rename-button
                  ^Button
                  merge-button
                  ^Button
                  delete-button,
                  action-toolbar,
                  apply-button
                  cancel-button]} (jfx/control-children control),
          window (doto (jfx/modal-window window-title, control-node)
                   (jfx/user-data! {:dialog-data dialog-data, :table-data table-data})
                   (t/add-avatar-icon))]

      (add-watch dialog-data :watch-entity-map
        (fn [_, _, old-map, new-map]
          (when-not (= (:entity-map old-map) (:entity-map new-map))
            (reset! table-data (create-table-data entity-count-fn, (:entity-map new-map))))))


      (jfx/setup-table-view! group-tableview, [:entity, :count], table-data, :sort? true)
      (jfx-select/enable-multiple-selection! group-tableview)
      (jfx/enable-copy-to-clipboard! group-tableview)

      (jfx/column-captions! group-tableview, [entity-column-name, count-column-name])
      ; compare entity names case insensitive
      (jfx/column-comparator! group-tableview, 0, (fn [s1, s2] (compare (str/lower-case s1) (str/lower-case s2))))

      (let [selected-groups-prop (jfx/functional-property
                                   (partial mapv :entity)
                                   (jfx/selected-rows-property group-tableview))]

        ; disable condition rename-button
        (jfx/bind (jfx/property rename-button, :disable)
          (jfx/functional-property
            #(-> % count (not= 1))
            selected-groups-prop))

        ; disable condition merge-button
        (jfx/bind (jfx/property merge-button, :disable)
          (jfx/functional-property
            #(-> % count (< 2))
            selected-groups-prop))

        ; disable condition delete-button
        (jfx/bind (jfx/property delete-button, :disable)
          (jfx/functional-property
            #(-> % count (= 0))
            selected-groups-prop))


        (jfx/handle-event! rename-button, :action,
          (fn [_]
            (let [selected-groups (jfx/value selected-groups-prop)]
              (jfx-select/clear-selection group-tableview)
              (rename-group entity-merge-fn, dialog-data, (first selected-groups)))))


        (jfx/handle-event! merge-button, :action,
          (fn [_]
            (let [selected-groups (jfx/value selected-groups-prop)]
              (if-let [error (when merge-predicate (merge-predicate (deref dialog-data), selected-groups))]
                (let [{:keys [title, message, content]} error]
                  (jfx/show-error title, message, content))
                (do
                  (jfx-select/clear-selection group-tableview)
                  (merge-groups entity-count-fn, entity-merge-fn, dialog-data, selected-groups))))))

        (jfx/handle-event! delete-button, :action,
          (fn [_]
            (let [selected-groups (jfx/value selected-groups-prop)]
              (jfx-select/clear-selection group-tableview)
              (delete-groups dialog-data, selected-groups))))

        (when additional-button
          (let [{:keys [text, icon, tooltip handler, disable?]} additional-button,
                btn (Button. text)]

            (jfx/bind (jfx/property btn, :disable)
              (jfx/functional-property
                (fn [selected-groups]
                  (disable? (count selected-groups)))
                selected-groups-prop))

            (when icon
              (.setGraphic btn, (MaterialDesignIconView. icon)))

            (when-not (str/blank? tooltip)
              (jfx/tooltip! btn, tooltip))

            (jfx/handle-event! btn, :action,
              (fn [_]
                (handler dialog-data, (jfx/value selected-groups-prop))))

            (jfx/add-to-toolbar action-toolbar, btn))))


      (jfx/handle-event! apply-button, :action,
        (fn [_]
          (complete-fn (:renaming-map (deref dialog-data)))
          (jfx/close window)))

      (jfx/handle-event! cancel-button, :action,
        (fn [_]
          (jfx/close window)))

      (jfx/show window))))


(defn show-dialog
  [ui, group-type, group->size-map, complete-fn]
  (table-edit-dialog
    {:window-title (format "Manage %s groups" (name group-type)),
     :entity-map group->size-map,
     :entity-count-fn identity,
     :entity-merge-fn +,
     :entity-column-name "group",
     :count-column-name "size"}
    (comp complete-fn u/remove-identities)))