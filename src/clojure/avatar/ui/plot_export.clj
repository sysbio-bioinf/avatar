; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.plot-export
  (:require [avatar.data.management :as dm]
            [avatar.ui.svg-export :as svg]
            [clj-jfx.core :as jfx]
            [avatar.ui.plot :as plot]
            [avatar.util :as u]
            [avatar.ui.dialogs.progress-view :as progress]))






(defn save-plot-as-snapshot
  [data-mgmt, plot]
  (when-let [snapshot-name (jfx/text-input "Save Snapshot", "Save the selected plot", "Enter the snapshot name:")]
    (dm/save-snapshot data-mgmt, snapshot-name, (plot/data plot), (plot/shown-alteration-type plot))))