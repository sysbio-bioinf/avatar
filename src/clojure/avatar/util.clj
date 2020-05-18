; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.util
  (:require
    [clojure.java.io :as io]
    [clojure.stacktrace :as st]
    [clojure.string :as str]
    [clojure.tools.logging :as log]
    [clojure.data :as data]
    [clojure.set :as set]
    [clojure.data.int-map :as im]
    [clj-jfx.util :as ju])
  (:import (java.util Date Locale Scanner)
           (java.text SimpleDateFormat NumberFormat)
           (clojure.lang IReduceInit)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream TarArchiveEntry)
           (java.util.zip GZIPInputStream)
           (java.nio.file Path Files CopyOption StandardCopyOption)
           (java.time ZoneId ZonedDateTime)))


(defn illegal-argument
  [fmt, & args]
  (throw (IllegalArgumentException. ^String (apply format fmt args))))


(defn runtime-exception
  [fmt, & args]
  (throw (RuntimeException. ^String (apply format fmt args))))


(defn trim-nil
  "Trims non-blank strings and returns nil for blank strings."
  [s]
  (when-not (str/blank? s)
    (str/trim s)))


(defn for-each!
  [f, coll]
  (reduce
    (fn [_, x]
      (f x)
      nil)
    nil
    coll))


(defn for-each-indexed!
  [f, coll]
  (reduce
    (fn [i, x]
      (f i, x)
      (inc i))
    0
    coll))


(defn for-each-kv!
  [f, coll]
  (reduce-kv
    (fn [_, k, v]
      (f k, v)
      nil)
    nil
    coll))


(defn reduce-indexed
  [f, val, coll]
  (let [index (volatile! 0)]
    (reduce
      (fn [result, value]
        (let [result (f result, @index, value)]
          (vswap! index unchecked-inc)
          result))
      val,
      coll)))


(defn reduce-kv-indexed
  [f, init, coll]
  (let [index (volatile! 0)]
    (reduce-kv
      (fn [result, key, value]
        (let [result (f result, @index, key, value)]
          (vswap! index unchecked-inc)
          result))
      init,
      coll)))


(defn iteration
  [^long k, f, x]
  (loop [i 0, x x]
    (if (< i k)
      (recur (unchecked-inc i), (f x))
      x)))


(defn iteration-indexed
  [^long k, f, x]
  (loop [i 0, x x]
    (if (< i k)
      (recur (unchecked-inc i), (f i, x))
      x)))


(defn keepv
  [f, coll]
  (persistent!
    (transduce
      (keep f)
      conj!
      (transient [])
      coll)))


(defn some-index
  [pred?, coll]
  (reduce-indexed
    (fn [_, index, x]
      (when (pred? x)
        (reduced index)))
    nil
    coll))


(defn some-value
  [pred?, coll]
  (reduce
    (fn [_, x]
      (when (pred? x)
        (reduced x)))
    nil
    coll))


(defn some-value-kv
  [pred?, map]
  (reduce-kv
    (fn [_, k, v]
      (when (pred? k v)
        (reduced [k, v])))
    nil
    map))


(defn update-in!
  "Straight forward port of clojure.core/update-in for transients"
  [m [k & ks] f & args]
  (if ks
    (assoc! m k (apply update-in (get m k) ks f args))
    (assoc! m k (apply f (get m k) args))))


(defn merge-with!
  [f, m1, m2]
  (reduce-kv
    (fn [result-map, k, v2]
      (let [v1 (get result-map k ::NOT-FOUND)]
        (if (= v1 ::NOT-FOUND)
          (assoc! result-map k v2)
          (assoc! result-map k (f v1 v2)))))
    m1
    m2))


(defn update!
  "Straight forward port of clojure.core/update for transients"
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f x]
   (assoc! m k (f (get m k) x)))
  ([m k f x y]
   (assoc! m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc! m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc! m k (apply f (get m k) x y z more))))


(defn contains!?
  [m k]
  (not= (get m k ::NOT-FOUND) ::NOT-FOUND))


(defn inverse-map
  [m]
  (persistent!
    (reduce-kv
      (fn [result-map, k, v]
        (update! result-map v (fnil conj #{}) k))
      (transient {})
      m)))


(defn inverse-bit-map
  [m]
  (persistent!
    (reduce-kv
      (fn [result-map, k, v]
        (update! result-map v (fnil conj (im/dense-int-set)) k))
      (transient {})
      m)))


(defn update-map-vals
  [m, f]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k (f v)))
      (transient {})
      m)))


(defn reduce-map
  [m, f]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (assoc! m k (f v)))
      (transient {})
      m)))


(defn filter-map
  [m, pred]
  (persistent!
    (reduce-kv
      (fn [m, k, v]
        (cond-> m
          (pred k, v)
          (assoc! k v)))
      (transient {})
      m)))


(defn zipmap-by
  [key-kw, value-kw, coll]
  (persistent!
    (reduce
      (fn [m, item]
        (assoc! m (get item key-kw) (get item value-kw)))
      (transient {})
      coll)))



(defn println-err
  [& msg]
  (binding [*out* *err*]
    (apply println msg)
    (flush)))


(defn print-cause-trace
  [throwable]
  (binding [*out* *err*]
    (st/print-cause-trace throwable)
    (flush)))


(def exceptions-on-stderr false)


(defn log-exception
  [message, e]
  (let [log-msg (str message "\n" (with-out-str (st/print-cause-trace e)))]
    (log/error log-msg)
    (when exceptions-on-stderr
      (println-err log-msg))))


(defn log-error
  [fmt & args]
  (let [msg (apply format fmt args)]
    (log/error msg)
    (when exceptions-on-stderr
      (println-err msg))))


(defn log-warn
  [fmt & args]
  (let [msg (apply format fmt args)]
    (log/warn msg)
    (when exceptions-on-stderr
      (println-err msg))))


(defn log-debug
  [fmt & args]
  (let [msg (apply format fmt args)]
    (log/debug msg)
    (when exceptions-on-stderr
      (println-err msg))))


(defmacro safe
  [& body]
  (let [message (str "Uncaught exception in " (pr-str &form))]
    `(try
       ~@body
       (catch Throwable t#
         (log-exception ~message, t#)))))


(defmacro safe-future
  "A Clojure future that prints exceptions to stderr."
  [& body]
  `(future (safe ~@body)))


(defn range-incl
  "Returns a vector containing the range of numbers specified by from and to"
  [^long from, ^long to]
  (let [forward? (<= from to),
        step (if forward? 1 -1)
        cmp (if forward? <= >=)]
    (loop [i from, result (transient [])]
      (if (cmp i to)
        (recur (+ i step), (conj! result i))
        (persistent! result)))))


(defn ->vector
  [xs]
  (cond
    (vector? xs) xs
    (or (sequential? xs) (set? xs)) (vec xs)
    :else (vector xs)))


(defn ->set
  [xs]
  (cond
    (set? xs) xs
    (sequential? xs) (set xs)
    :else (conj #{} xs)))


(defn into!
  [to, from]
  (if (instance? clojure.lang.Indexed from)
    (let [n (count from)]
      (loop [i 0, to to]
        (if (< i n)
          (recur (unchecked-inc i), (conj! to (nth from i)))
          to)))
    (reduce
      conj!
      to
      from)))


(defn configure-logging
  "Configures the logging for TraqBio. Log level and log file can be specified in the configuration."
  [{:keys [log-level, log-file] :as config}]
  (let [props (doto (System/getProperties)
                (.setProperty "log4j.rootLogger" (format "%s, file" (-> log-level name str/upper-case)))
                (.setProperty "log4j.appender.file" "org.apache.log4j.RollingFileAppender")
                (.setProperty "log4j.appender.file.File" (str log-file))
                (.setProperty "log4j.appender.file.MaxFileSize" "4MB")
                (.setProperty "log4j.appender.file.MaxBackupIndex" "5")
                (.setProperty "log4j.appender.file.layout" "org.apache.log4j.PatternLayout")
                (.setProperty "log4j.appender.file.layout.ConversionPattern" "%d{yyyy.MM.dd HH:mm:ss} %5p %c: %m%n"))]
    (org.apache.log4j.PropertyConfigurator/configure props))
  nil)


(defn format-duration
  [^double duration, ^long digits]
  (let [quotients [1, 1E3, 1E6, 1E9]
        units ["ns" "µs" "ms" "s"]
        unit-index (loop [i 0]
                     (if (and
                           (< i (count quotients))
                           (>= (long (/ duration (nth quotients i))) 1))
                       (recur (inc i))
                       (dec i)))]
    (format (str "%." digits "f " (nth units unit-index)) (/ duration (nth quotients unit-index)))))


(def ^:dynamic *timing-enabled* true)


(defmacro timing
  [context-description, & body]
  `(if *timing-enabled*
     (let [start# (System/nanoTime)
           result# (do ~@body)
           stop# (System/nanoTime)]
       (log/tracef "time(%s) = %s" ~context-description (format-duration (- stop# start#), 3))
       result#)
     (do ~@body)))


(defn inverse-permutation
  [permutation]
  (persistent!
    (reduce-kv
      (fn [inverse, i, s_i]
        (assoc! inverse s_i i))
      (transient permutation)
      permutation)))


(defn file-extension
  [file]
  (some->> file io/file .getName (re-matches #".*\.(.*)$") second))


(defn file->path
  ^Path [file]
  (some-> file io/file .toPath))


(defn file-copy
  [source, destination]
  (Files/copy (file->path source), (file->path destination), ^"[Ljava.nio.file.CopyOption;" (into-array [StandardCopyOption/REPLACE_EXISTING])))


(defn download-file
  [url, destination]
  (Files/copy (io/input-stream url), (file->path destination), ^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))


(defn import-file-type
  [file]
  (if (contains? #{"xls" "xlsx"} (-> file file-extension str/lower-case))
    :excel
    :csv))


(defn column-separator
  "Determines the column separator from the file extension."
  [file]
  (case (str/lower-case (file-extension file))
    "csv" \,
    "ssv" \;
    "tsv" \tab))


(defn file-absolute-path
  [file]
  (-> file io/file .getAbsolutePath))


(defn file-name
  [file]
  (-> file io/file .getName))


(defn safe-nth
  ([coll, ^long n]
   (safe-nth coll, n, nil))
  ([coll, ^long n, default]
   (if (< n (count coll))
     (nth coll n)
     default)))


(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (mapv #(with-meta % nil) (keys &env))]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))


(defn print-locals-map
  [locals-map]
  (println-err "LOCALS:")
  (->> locals-map
    (sort-by key)
    (map (fn [[s, v]] (str s " = " v)))
    (str/join "\n")
    println-err)
  (println-err)
  (flush))


(defmacro debug
  [& body]
  `(try
     ~@body
     (catch Throwable ~'the-caught-exception
       (print-locals-map (dissoc (local-bindings) '~'the-caught-exception))
       (throw ~'the-caught-exception))))


(defn current-timestamp
  []
  (let [date (Date.)]
    (.format (SimpleDateFormat. "yyyy.MM.dd\nHH:mm:ss") date)))


(defmacro case+
  "A case macro that fills in the values of static class attributes that are used as test constants."
  [expr, & clauses]
  (let [pairs (partition 2 clauses)
        clauses (cond-> (->> pairs
                          (mapcat
                            (fn [[test-expr, do-expr :as clause]]
                              (if (and (symbol? test-expr) (nil? (resolve test-expr)))
                                [(eval test-expr), do-expr]
                                clause)))
                          vec)
                  (odd? (count clauses))
                  (conj (last clauses)))]
    `(case ~expr ~@clauses)))


(defn object-comparator
  "Compares two given objects by their reference (approximated via System/identityHashCode)"
  [a, b]
  (Integer/compare (System/identityHashCode a), (System/identityHashCode b)))


(defn delete
  "Delete the values given in the index set from the specified vector."
  [index-set, data-vec]
  (let [index-set (->set index-set),
        data-vec (->vector data-vec),
        n (count data-vec)]
    (loop [i 0, result-vec (transient [])]
      (if (< i n)
        (recur
          (unchecked-inc i),
          (cond-> result-vec (not (contains? index-set i)) (conj! (nth data-vec i))))
        (persistent! result-vec)))))


(defn filter-by-index
  "Keep only the values given in the index set from the specified vector."
  [index-set, data-vec]
  (let [index-set (->set index-set),
        data-vec (->vector data-vec),
        n (count data-vec)]
    (loop [i 0, result-vec (transient [])]
      (if (< i n)
        (recur
          (unchecked-inc i),
          (cond-> result-vec (contains? index-set i) (conj! (nth data-vec i))))
        (persistent! result-vec)))))


(defn mapv-indexed
  [f, v]
  (let [v (->vector v),
        n (count v)]
    (loop [i 0, result (transient [])]
      (if (< i n)
        (recur
          (unchecked-inc i),
          (conj! result (f i, (nth v i))))
        (persistent! result)))))


(defn round-to
  ^double [^long digits, ^double x]
  (let [factor (iteration digits #(* 10.0 %) 1.0)]
    (-> x (* factor) Math/round (/ factor))))


(defn keep-max
  [& values]
  (when-let [values (seq (remove nil? values))]
    (reduce max values)))


(defn reduce-max
  [f, coll]
  (when-let [values (seq (keep f coll))]
    (reduce max values)))





(defn equality-partition-diff-case
  [a, b, indent, parent-key]
  (let [ep-a (or (data/equality-partition a) :atom)
        ep-b (or (data/equality-partition b) :atom)]
    (if (= ep-a ep-b)
      ep-a
      :atom)))


(defn indent-str
  [len]
  (apply str (repeat len " ")))


(defmulti differences #'equality-partition-diff-case)


(defn value-str
  [x]
  (if (= x ::NA)
    "∅"
    (pr-str x)))

(defn parent-key-str
  [parent-key]
  (when parent-key
    (format "[%s]  " parent-key)))


(defmethod differences :atom
  [a, b, indent, parent-key]
  (when-not (= a b)
    (str (indent-str indent)
      (parent-key-str parent-key)
      (format "%s -> %s" (value-str a), (value-str b)))))


(defn compare-anything
  [a, b]
  (let [ca (class a),
        cb (class b)]
    (if (= ca cb)
      (compare a b)
      (compare (.getCanonicalName ca), (.getCanonicalName cb)))))


(defn differences-for-keys
  [a, b, indent, parent-key, ks]
  (when-not (= a b)
    (let [diff-strs (keep
                      (fn [k]
                        (let [va (get a k ::NA),
                              vb (get b k ::NA)]
                          (when-not (= va vb)
                            (differences va, vb, (+ indent 2), k))))
                      ks)]
      (str/join "\n"
        (cond->> diff-strs
          parent-key (list* (str (indent-str indent) (parent-key-str parent-key))))))))


(defmethod differences :sequential
  [a, b, indent, parent-key]
  (differences-for-keys a, b, indent, parent-key, (range (max (count a) (count b)))))


(defmethod differences :map
  [a, b, indent, parent-key]
  (differences-for-keys a, b, indent, parent-key,
    (->> (concat (keys a) (keys b))
      distinct
      (sort compare-anything))))


(defmethod differences :set
  [a, b, indent, parent-key]
  (when-not (= a b)
    (str (indent-str indent)
      (parent-key-str parent-key)
      (format "%s -> %s" (pr-str (set/difference a, b)) (pr-str (set/difference b, a))))))


(defn print-differences
  [a, b]
  (println (differences a, b, -2, nil)))


(defn print-modifications
  ([old-value, new-value]
   (print-modifications nil, old-value, new-value))
  ([context, old-value, new-value]
   (when context
     (println context))
   (let [recursive-print (fn recursive-print [ident, parent-key, old, new]
                           (let [ident-str (when parent-key
                                             (str (apply str (repeat ident " ")) (format "[%s] " (pr-str parent-key))))]
                             (cond
                               (and (map? old) (map? new)) (let [[only-old, only-new] (data/diff old, new),
                                                                 strs (->> (keys only-old)
                                                                        (concat (keys only-new))
                                                                        (into (sorted-set-by ju/universal-compare))
                                                                        (map (fn [k] (recursive-print (+ ident 2) k (get only-old k) (get only-new k)))))]
                                                             (str/join "\n" (cond->> strs ident-str (list* ident-str))))
                               (and (sequential? old) (sequential? new)) (let [[only-old, only-new] (data/diff old, new)
                                                                               only-old-count (count only-old),
                                                                               only-new-count (count only-new),
                                                                               strs (->> (range (max only-old-count only-new-count))
                                                                                      (keep
                                                                                        (fn [index]
                                                                                          (let [old-val (when (< index only-old-count) (nth only-old index)),
                                                                                                new-val (when (< index only-new-count) (nth only-new index))]
                                                                                            (when (or (some? old-val) (some? new-val))
                                                                                              (recursive-print (+ ident 2), index, old-val, new-val))))))]
                                                                           (str/join "\n" (cond->> strs ident-str (list* ident-str))))
                               :else (format (str ident-str "%s -> %s") old, new))))]
     (println (recursive-print -2, nil, old-value, new-value)))))


(defn watch-state-changes
  ([atom-or-ref]
   (watch-state-changes nil, atom-or-ref))
  ([context, atom-or-ref]
   (add-watch atom-or-ref ::watch-state-changes
     (fn [_, _, old-state, new-state]
       (when-not (= old-state new-state)
         (println)
         (when context
           (println context))
         (print-modifications old-state, new-state))))))


(defn watch-new-states
  ([atom-or-ref]
   (watch-new-states nil, atom-or-ref))
  ([context, atom-or-ref]
   (add-watch atom-or-ref ::watch-new-states
     (fn [_, _, _, new-state]
       (println)
       (when context
         (println context))
       (prn new-state)))))


(defn ->print
  ([x, message]
   (->print x, message, identity))
  ([x, message, f]
   (println (format "%s: %s" message (pr-str (f x))))
   x))


(defn ->>print
  ([message, x]
   (->>print message, identity, x))
  ([message, f, x]
   (println (format "%s: %s" message (pr-str (f x))))
   x))


(defn deep-merge
  [& maps]
  (apply merge-with
    (fn [x y]
      (if (and (map? x) (map? y))
        (deep-merge x y)
        y))
    maps))


(defn deep-merge-non-delete
  "Returns a deep merge of the given maps where nil values in latter maps do not delete the data."
  [& maps]
  (apply merge-with
    (fn [x y]
      (if (and (map? x) (map? y))
        (deep-merge-non-delete x y)
        (if (nil? y) x y)))
    maps))


(defn deep-merge-append-only
  "Returns a deep merge of the given maps where existing keys are not replaced."
  [& maps]
  (apply merge-with
    (fn [x y]
      (if (and (map? x) (map? y))
        (deep-merge-append-only x y)
        (or x y)))
    maps))


#_(defn write-random-csv-data
    [sample-count, gene-count]
    (->> (range gene-count)
      (map
        (fn [i]
          (->> (repeatedly sample-count
                 #(if (< (rand) 0.2)
                    "M"
                    "WT"))
            (list* (Utils/getExcelLetterFromNumber i))
            (str/join ";"))))
      (list* (str/join ";" (list* "Samples" (range sample-count))))
      (str/join "\n")
      (spit (format "random-test-data-p%d-g%d.csv" sample-count gene-count))))


(defn extension-map
  [& extensions]
  (reduce
    (fn [m, ext]
      (let [upper-case-ext (str/upper-case ext)]
        (assoc m
          (format "%1$s image (*.%2$s, *.%1$s)", upper-case-ext, ext)
          [(str "*." ext) (str "*." upper-case-ext)])))
    {}
    extensions))


(defn flat-map-diff
  "Compares two maps and returns a set of the keys whose values are different in the given maps."
  [old-map, new-map]
  (let [all-keys (-> (keys old-map)
                   (concat (keys new-map))
                   distinct
                   vec)]
    (persistent!
      (reduce
        (fn [diff-set, key]
          (let [old-val (get old-map key),
                new-val (get new-map key)]
            (cond-> diff-set
              (not (or (identical? old-val new-val) (= old-val new-val)))
              (conj! key))))
        (transient #{})
        all-keys))))


(let [locale Locale/ENGLISH,
      fmt (doto (NumberFormat/getInstance locale)
            (.setMinimumFractionDigits 1)
            (.setGroupingUsed false))]

  (defn format-double
    ^String [^double x]
    (.format fmt x))

  (defn parse-double
    ^double [^String s]
    (.parse fmt s))

  (defn is-double?
    [^String s]
    (let [scanner (doto (Scanner. s) (.useLocale locale))]
      (.hasNextDouble scanner)))

  (defn is-long?
    [^String s]
    (let [scanner (doto (Scanner. s) (.useLocale locale))]
      (.hasNextLong scanner))))


(defn parse-long
  ^long [text]
  (Long/parseLong text))


(defn print-gene-names
  [gene-coll]
  (->> gene-coll
    (mapv (fn [{:keys [gene, group]}] (if (str/blank? gene) group gene)))
    (str/join " ")
    println))


(defn home-folder
  []
  (System/getProperty "user.home"))


(defn file-in-home-folder
  [filename]
  (.getPath (io/file (home-folder) filename)))



(defn tar-entry-name
  [^TarArchiveEntry entry]
  (.getName (io/file (.getName entry))))


(defn gzip-input-stream
  [url]
  (GZIPInputStream. (io/input-stream url)))


(defn tar-gz-entries
  [archive-url]
  (reify
    IReduceInit
    (reduce [this, f, start]
      (let [in (TarArchiveInputStream. (gzip-input-stream archive-url))]
        (try
          (loop [result start]
            (if (.getNextEntry in)
              (let [entry (.getCurrentEntry in)
                    result (f result, {:input-stream in, :entry entry, :entry-name (tar-entry-name entry)})]
                (if (reduced? result)
                  (deref result)
                  (recur result)))
              result))
          (finally
            (.close in)))))))


(defn keepcat
  [f, coll]
  (persistent!
    (reduce
      (fn [result-vec, e]
        (into! result-vec (f e)))
      (transient [])
      coll)))


(defn removev
  [pred, coll]
  (into []
    (remove pred)
    coll))


(defn normalize-values
  [^double max-value, value-map]
  (persistent!
    (reduce-kv
      (fn [result-map, key, value]
        (assoc! result-map key (/ value max-value)))
      (transient {})
      value-map)))


(defn file-exists?
  [file]
  (if (instance? Path file)
    (.exists (.toFile ^Path file))
    (.exists (io/file file))))


(defn url-exists?
  [url]
  (try
    (-> (io/as-url url) .openStream .close)
    true
    (catch Exception _
      false)))


(defn cpu-thread-count
  []
  (.availableProcessors (Runtime/getRuntime)))


(defn duration-data
  [^long duration-in-millis]
  (let [milliseconds (mod duration-in-millis 1000),
        duration-in-secs (quot duration-in-millis 1000),
        seconds (mod duration-in-secs 60),
        duration-in-mins (quot duration-in-secs 60),
        minutes (mod duration-in-mins 60),
        duration-in-hours (quot duration-in-mins 60),
        hours (mod duration-in-hours 24),
        days (quot duration-in-hours 24)]
    {:milliseconds milliseconds,
     :seconds seconds,
     :minutes minutes,
     :hours hours,
     :days days}))


(defn quoted-str
  [s]
  (if s
    (format "\"%s\"" s)
    ""))


(defn transpose
  [rows]
  (apply mapv vector rows))


(defn remove-identities
  "Removes key-value-pairs from the map where key and value are equal."
  [m]
  (persistent!
    (reduce-kv
      (fn [result-map, k, v]
        (cond-> result-map
          (not= k v)
          (assoc! k v)))
      (transient {})
      m)))


(defn normalize-str
  [s]
  (some-> s str/trim str/lower-case))


(defn amend-filename
  [file, insertion]
  (let [file (io/file file),
        path (.getParent file),
        name+ext (.getName file)
        [_ name ext] (re-matches #"(.*)\.([^.]+)$" name+ext)]
    (io/file path, (str name insertion "." ext))))


(defn set-difference
  [s1, s2]
  (persistent! (reduce disj! (transient s1) s2)))


(defn key-map
  [key-fn, coll]
  (persistent!
    (reduce
      (fn [result-map, e]
        (assoc! result-map (key-fn e) e))
      (transient {})
      coll)))

(defn key-value-map
  [key-fn, value-fn, coll]
  (persistent!
    (reduce
      (fn [result-map, e]
        (assoc! result-map (key-fn e) (value-fn e)))
      (transient {})
      coll)))


(defn intersection-fast
  [set-1, set-2]
  ; ensure that set-2 is smaller
  (let [[set-1, set-2] (if (< (count set-1) (count set-2))
                         [set-2, set-1]
                         [set-1, set-2])]
    (persistent!
      (reduce
        (fn [intersection-set, e]
          (cond-> intersection-set
            ; duplicate?
            (contains? set-1 e)
            (conj! e)))
        (transient #{})
        set-2))))


(defn current-year
  []
  (.getYear (ZonedDateTime/now (ZoneId/systemDefault))))