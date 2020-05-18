; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(if-let [init-fn (resolve 'clj-jfx.init/init)]
  (init-fn)
  (do
    (require 'clj-jfx.init)
    ((resolve 'clj-jfx.init/init))))

(ns dev
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]
    [clojure.repl :refer [apropos dir doc find-doc pst source]]
    #_[clj-memory-meter.core :as mm]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh refresh-all]]
    [com.stuartsierra.component :as c]
    [clojure.data :refer [diff]]
    [debug.inspect :as di]
    [debug.reflect :refer [reflect]]
    [clj-jfx.core :as jfx]
    [clj-jfx.font-selection :as font]
    [avatar.ui.plot :as plot]
    [avatar.ui.application :as app]
    [avatar.ui.dialogs.import-matrix-file :as imp-dia]
    [avatar.ui.dialogs.import-bioportal :as imp-bp]
    [avatar.ui.dialogs.progress-view :as progress]
    [avatar.ui.dialogs.gene-symbol-normalization :as gsn]
    [avatar.data.management :as dm]
    [avatar.data.operations :as ops]
    [avatar.data.import :as imp]
    [avatar.data.bioportal :as bp]
    [avatar.data.geo :as geo]
    [avatar.data.gene-symbol-synonyms :as biodb]
    [avatar.data.conversions :as conv]
    [avatar.algorithms.greedy-setcover-sort :as new-greedy]
    [avatar.algorithms.column-sort :as cs]
    [avatar.algorithms.score :as score]
    [avatar.util :as u]
    [avatar.ui.svg :as svg]
    [avatar.ui.svg-export :as svg-new]
    [avatar.ui.tools :as t]
    [avatar.data.csv :as csv])
  (:import (clojure.lang Var)
           (java.lang.management ManagementFactory)
           (java.util Properties)))


(jfx/implicit-exit! false)


(intern 'clojure.core 'inspect di/inspect)
(intern 'clojure.core 'inspect-result di/inspect-result)

(def system-config
  {:ui {:implicit-exit? false}})


(defonce system (atom nil))


(defn current-main-window
  [system]
  (let [primary-stage-fn (resolve `t/primary-stage)]
    (-> system deref :ui primary-stage-fn)))

(defn current-plot
  [system]
  (let [current-plot-fn (resolve `app/current-plot)]
    (-> system deref :ui current-plot-fn)))

(defn current-data
  [system]
  (let [current-data-fn (resolve `dm/current-data)]
    (-> system deref :data-management current-data-fn dosync)))


(defn current-mgmt
  [system]
  (-> system deref :data-management))

(defn current-style
  [system]
  (let [plot-style-fn (resolve `dm/plot-style)]
    (-> system deref :data-management plot-style-fn dosync)))

(defn history
  [system]
  (let [history-fn (resolve `dm/history)]
    (-> system deref :data-management history-fn dosync)))

(defn load-from-file
  [data-mgmt, file]
  ((resolve `app/load-from-file) data-mgmt, file))


(defn init
  "Creates and initializes the system under development in the Var
  #'system."
  []
  (u/configure-logging {:log-level :debug, :log-file "avatar.log"})
  (alter-var-root #'u/exceptions-on-stderr (constantly true))
  (swap! system (constantly ((resolve 'avatar.system/create-system) system-config))))


(defn start
  "Starts the system running, updates the Var #'system."
  []
  (swap! system c/start)
  (doto system
    (-> deref :ui (load-from-file "test.avatar"))))


(defn stop
  "Stops the system if it is currently running, updates the Var
  #'system."
  []
  (swap! system #(when % (c/stop %))))


(defn go
  "Initializes and starts the system running."
  []
  (init)
  (start)
  :ready)


(defn reset
  "Stops the system, reloads modified source files, and restarts it."
  []
  (stop)
  (let [result (refresh :after `go)]
    (when (instance? Throwable result)
      (u/print-cause-trace result))))


(defn var-name
  [^Var var]
  (let [namespace (.name (.ns var)),
        symbol (.sym var)]
    (format "%s/%s" (name namespace) (name symbol))))

(defn wrap-timing
  [fn-var]
  (assert (var? fn-var) "must be a variable")
  (let [timing-data (atom [])]
    (alter-var-root fn-var
      (fn [f]
        (if (ifn? f)
          (fn [& xs]
            (let [start (System/currentTimeMillis)
                  y (apply f xs)
                  stop (System/currentTimeMillis)]
              (swap! timing-data conj (- stop start))
              y))
          (do
            (println (var-name fn-var) "is not bound to a function")
            f))))
    (alter-meta! fn-var assoc ::timing-data timing-data))
  fn-var)


(defn report-timing
  [fn-var]
  (assert (var? fn-var) "must be a variable")
  (let [fn-name (var-name fn-var)]
    (let [timings (->> fn-var meta ::timing-data deref)]
      (if (seq timings)
        (let [sum (reduce + 0 timings)]
          (println
            (format "%s\n\nmean runtime: %.3fms\ncall count: %d\ntotal runtime: %dms\nmin runtime: %dms\nmax runtime: %dms"
              fn-name
              (/ (double sum) (count timings))
              (count timings)
              sum
              (reduce min timings)
              (reduce max timings))))
        (println (format "no timing data for variable %s" fn-name))))))


(defn print-process-id
  []
  (.getName (ManagementFactory/getRuntimeMXBean)))



(defn memory-usage
  []
  (let [rt (Runtime/getRuntime)
        free (.freeMemory rt),
        total (.totalMemory rt),]
    {:free free
     :used (- total free)
     :total total}))


(defn print-memory-usage
  "Prints the current memory usage consisting of used memory, total memory and percentage of free memory."
  []
  (let [{:keys [free, total]} (memory-usage),
        free-ratio (/ (double free) total)]
    (println
      (format "%3.3f GB of %3.3f GB (free: %3.2f%%)"
        (/ (* (- 1 free-ratio) total) 1024.0 1024 1024)
        (/ total 1024.0 1024 1024)
        (* 100 free-ratio)))))


(defn gc
  "Perform garbage collection and print memory changes."
  []
  (let [{used-prev :used, total-prev :total} (memory-usage)
        _ (System/gc)
        {:keys [used, total]} (memory-usage)
        ->mb (fn [^double x] (/ x 1024.0 1024.0))]
    (println
      (format "Garbage collection: freed %3.3f MB, reduced total by %3.3f MB (%3.3f MB of %3.3f MB)"
        (->mb (- used-prev used))
        (->mb (- total-prev total))
        (->mb used)
        (->mb total)))))


(defn read-gene-sets
  [file]
  (->> (csv/read-csv file, :separator ";")
    u/transpose
    (reduce
      (fn [result-map, [group, & genes]]
        (assoc result-map group (set (remove str/blank? genes))))
      {})))


(defn gene-set-comparison-table
  [gene-sets-map]
  (let [all-genes (reduce-kv
                    (fn [result-set, _, gene-set]
                      (into result-set gene-set))
                    #{}
                    gene-sets-map),
        gene-set-list (->> gene-sets-map keys sort vec)]
    (->> all-genes
      sort
      (mapv
        (fn [gene]
          (->> gene-set-list
            (mapv
             (fn [gene-set]
               (if (get-in gene-sets-map [gene-set, gene])
                 "x"
                 "")))
            (list* gene))))
      (list* (list* "gene" gene-set-list)))))


(comment
  (csv/write-csv "gene-sets-table.csv" (gene-set-comparison-table (read-gene-sets "/home/gv/Simon-CTA-Use-Case-PDFs/gene-sets.csv")))
  )



(defn dependency-version
  [dep]
  (let [path (format "META-INF/maven/%s/%s/pom.properties"
               (or (namespace dep) (name dep))
               (name dep))
        props-file (io/resource path)]
    (when props-file
      (with-open [stream (io/input-stream props-file)]
        (let [props (doto (Properties.) (.load stream))]
          (.getProperty props "version"))))))