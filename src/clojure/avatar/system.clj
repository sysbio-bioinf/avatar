; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.system
  (:require
    [com.stuartsierra.component :as c]
    [avatar.ui.application :as ui]
    [avatar.data.management :as dm]
    [avatar.data.bioportal :as bp]
    [avatar.data.gene-symbol-synonyms :as gss]
    [avatar.util :as u]))


(defn create-system
  [config]
  (let [download-directory (u/file-in-home-folder ".avatar-downloads")
        geo-download-directory (u/file-in-home-folder ".avatar-downloads/geo")]
    (c/system-using
      (c/system-map
        :ui (ui/create-user-interface (assoc (:ui config) :geo-download-directory geo-download-directory))
        :data-management (dm/create-data-management)
        :bioportal (bp/create-bioportal download-directory)
        :synonym-service (gss/create-synonym-service))
      {:ui {:data-management :data-management,
            :bioportal :bioportal,
            :synonym-service :synonym-service}})))

