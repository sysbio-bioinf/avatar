; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.main
  (:gen-class
    :name avatar.Main)
  (:require
    [avatar.util :as u]
    [clojure.tools.logging :as log]
    [clojure.tools.cli :as cli]
    [clojure.string :as str]))






; TODO: switch default debug level back to :info
(def cli-options
  [["-h" "--help" "Shows this help."]
   ["-L" "--log-level L" "Specifies the log level to use." :parse-fn keyword, :default :debug #_:info, :validate [#{:all, :debug, :trace, :info, :warn, :error, :fatal, :off} "Must be one of all, debug, trace, info, warn, error, fatal, off!"]]
   ["-f" "--file DATA_FILE" "Specifies the file to load for the selected operation (e.g. batch optimization)."]
   ["-o" "--optimization BATCH_CONFIG" "Perform optimization runs with the specified batch config file."]
   ["-t" "--threads N" "Specify the number of threads that are run in parallel." :parse-fn #(Long/parseLong %), :default 1, :validate [pos?]]])


(defn -main
  [& args]
  (let [{:keys [options, arguments, summary, errors]} (cli/parse-opts args, cli-options),
        {:keys [log-level, help, file, optimization, threads]} options]
    (cond
      errors (do
               (println (format "ERRORS\n  * %s\n\nOPTIONS\n%s" (str/join "\n  *" errors) summary))
               (System/exit 1))
      help (do
             (println (format "OPTIONS\n%s" summary))
             (System/exit 0))
      optimization
            (do
              (u/configure-logging {:log-level log-level, :log-file "avatar.log"})
              (alter-var-root #'log/*logging-agent* (constantly :agent))
              (require '[avatar.commandline :as ic])
              ((resolve 'ic/run-batch-optimization) threads, optimization, file))
      :else (do
              (u/configure-logging {:log-level log-level, :log-file (u/file-in-home-folder "avatar.log")})
              (alter-var-root #'log/*logging-agent* (constantly :agent))
              (require '[avatar.gui-application :as gui])
              ((resolve 'gui/launch))))))
