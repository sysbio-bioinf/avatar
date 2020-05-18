; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.gui-application
  (:gen-class
    :name avatar.GuiApplication
    :extends javafx.application.Application)
  (:require [avatar.util :as u]
            [clj-jfx.splashscreen :as splash])
  (:import (javafx.application Application Platform)))



(defn -start
  [app, stage]
  (require '[clj-jfx.init :as init])
  ((resolve 'init/init))
  (future
    (let [splashscreen (splash/show-splashscreen "avatar/ui/Splashscreen.fxml")]
      (require
        '[com.stuartsierra.component :as c]
        '[avatar.system :as sys])
      ((resolve 'c/start) ((resolve 'sys/create-system) {:ui {:implicit-exit? true, :splashscreen splashscreen}})))))


(defn launch
  []
  (try
    (Application/launch (resolve 'avatar.GuiApplication) (into-array String []))
    (catch Throwable t
      (u/log-exception "Exception in GUI launch" t))))