; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.about-dialog
  (:require [clj-jfx.core :as jfx]
            [avatar.ui.tools :as t]
            [avatar.version :as v]
            [clj-jfx.properties :as props]
            [clojure.java.io :as io]
            [clj-jfx.table :as table]
            [avatar.util :as u])
  (:import (javafx.scene.control Hyperlink TableCell)
           (javafx.geometry Pos)
           (javafx.application HostServices Application)))



(defn license-cell-factory
  [_]
  (let [hyperlink (Hyperlink.)]
    (doto (table/table-cell
            (fn [^TableCell cell, item, empty?]
              (if empty?
                (.setGraphic cell nil)
                (do
                  (doto hyperlink
                    (.setText (:name item))
                    (.setOnAction (jfx/event-handler (fn [_] (jfx/show-document (:url item))))))
                  (.setGraphic cell hyperlink)))))
      (jfx/padding! 0))))


(defn show-dialog
  [ui]
  (let [control (jfx/create-control "avatar/ui/AboutDialog.fxml")
        about-dialog (jfx/control-node control)
        {:keys [version-label
                copyright-year-text
                epl-link
                licenses-table
                close-button]
         :as controls} (jfx/control-children control)
        license-data (read-string (slurp (io/resource "library-licenses.edn")))
        window (doto (jfx/modal-window "About" about-dialog)
                 (t/add-avatar-icon))]
    (jfx/label-text! version-label, (v/current-version))
    (props/set-property copyright-year-text, :text, (str (u/current-year)))

    (doto epl-link
      (jfx/padding! 0)
      (jfx/handle-event!
        :action
        (fn [_]
          (jfx/show-document "http://www.eclipse.org/legal/epl-v20.html"))))

    (table/setup-table licenses-table,
      [{:attribute :artifact
        :name "Library"
        :sortable? true
        :reorderable? false
        :pref-width 200}
       {:attribute :version
        :name "Version"
        :sortable? true
        :reorderable? false}
       {:attribute :license
        :name "License"
        :sortable? true
        :reorderable? false
        :comparator (fn [a, b] (compare (:name a) (:name b)))
        :cell-factory license-cell-factory}]
      {:sort? true})

    (props/set (table/rows-property licenses-table) license-data)

    (jfx/handle-event! close-button, :action, (fn [_] (.close window)))
    (jfx/show-and-wait window)))