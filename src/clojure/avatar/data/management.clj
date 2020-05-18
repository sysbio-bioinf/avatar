; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.data.management
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [com.stuartsierra.component :as c]
    [frost.quick-freeze :as qf]
    [avatar.data.serializers :as ds]
    [avatar.data.schema :as schema]
    [avatar.data.gene-data :as gd]
    [avatar.util :as u])
  (:import
    (java.io File)))


; colors from: http://stackoverflow.com/questions/470690/how-to-automatically-generate-n-distinct-colors
(def kelly-colors
  [; Vivid Yellow
   "#FFB300"
   ; Strong Purple
   "#803E75"
   ; Vivid Orange
   "#FF6800"
   ; Very Light Blue
   "#A6BDD7"
   ; Vivid Red
   "#C10020"
   ; Grayish Yellow
   "#CEA262"
   ; Medium Gray
   "#817066"
   ; Vivid Green
   "#007D34"
   ; Strong Purplish Pink
   "#F6768E"
   ; Strong Blue
   "#00538A"
   ; Strong Yellowish Pink
   "#FF7A5C"
   ; Strong Violet
   "#53377A"
   ; Vivid Orange Yellow
   "#FF8E00"
   ; Strong Purplish Red
   "#B32851"
   ; Vivid Greenish Yellow
   "#F4C800"
   ; Strong Reddish Brown
   "#7F180D"
   ; Vivid Yellowish Green
   "#93AA00"
   ; Deep Yellowish Brown
   "#593315"
   ; Vivid Reddish Orange
   "#F13A13"
   ; Dark Olive Green
   "#232C16"])


(defn default-plot-style []
  {:even-row-color "#DFDFDF",
   :odd-row-color "#A9A9A9",
   :even-column-group-color "#C9C9C9",
   :odd-column-group-color "#A9A9A9",
   :alteration-color "#000000",
   :overlap-color "#616161"
   :missing-value-color "#FFFFFF",
   :shown-alteration-type nil,
   :paint-missing-values? true,
   :border-color "#FFFFFF"
   :border-width 1.0,
   :alteration-cell-width 5,
   :alteration-ratio-digits 0,
   :fixed-row-header-color "lightgreen"
   :selection-color "#FFC125"
   :gene-group-font {:family "Arial", :weight :bold, :posture :regular, :size 13},
   :sample-group-font {:family "Arial", :weight :bold, :posture :regular, :size 13},
   :alteration-ratio-font {:family "Arial", :weight :normal, :posture :regular, :size 13},
   :min-gene-group-width 50,
   :min-row-height 30,
   :gene-name-separator-spacing 1.0,
   :gene-name-separator "-",
   :gene-name-left-font {:family "Arial", :weight :normal, :posture :italic, :size 13},
   :gene-name-right-font {:family "Arial", :weight :normal, :posture :regular, :size 13},
   :sample-groups-colored? false,
   :sample-group->color {}})


(defrecord DataManagement [data-ref, snapshots-ref, pareto-front-data-ref, plot-style-atom, current-file-atom, change-handlers-atom, notification-agent, dirty?-atom]

  c/Lifecycle

  (start [data-mgmt]
    (dosync
      (ref-set pareto-front-data-ref {:single {:saved [], :latest nil}, :batch []}))
    ; maybe reset state later
    data-mgmt)

  (stop [data-mgmt]
    (dosync
      (ref-set data-ref nil)
      (ref-set snapshots-ref [])
      (ref-set pareto-front-data-ref nil)
      (reset! plot-style-atom (default-plot-style))
      (reset! current-file-atom nil)
      (reset! change-handlers-atom [])
      (reset! dirty?-atom false))
    data-mgmt))



(defn create-data-management
  []
  (map->DataManagement
    {:data-ref (ref nil),
     :snapshots-ref (ref []),
     :pareto-front-data-ref (ref nil),
     :plot-style-atom (atom (default-plot-style)),
     :current-file-atom (atom nil),
     :change-handlers-atom (atom []),
     :notification-agent (agent 0),
     :dirty?-atom (atom false)}))


(defn history-pos
  [^DataManagement data-mgmt]
  (-> (.data-ref data-mgmt)
    ensure
    :position))


(defn history
  [^DataManagement data-mgmt]
  (-> (.data-ref data-mgmt)
    ensure
    :history))


(defn current-data
  [^DataManagement data-mgmt]
  (dosync
    (let [{:keys [position, history]} (ensure (.data-ref data-mgmt))]
      (when (seq history)
        (nth history position)))))


(defn dirty?
  "Returns true if the data management is marked as dirty.
  In this case there have been changes since the current state has been saved to file."
  [^DataManagement data-mgmt]
  (deref (.dirty?-atom data-mgmt)))

(defn dirty!
  "Set the state of the data managment to dirty."
  [^DataManagement data-mgmt]
  (reset! (.dirty?-atom data-mgmt) true)
  data-mgmt)


(defn clean!
  "Set the state of the data managment to clean."
  [^DataManagement data-mgmt]
  (reset! (.dirty?-atom data-mgmt) false)
  data-mgmt)


(defn plot-style
  [^DataManagement data-mgmt]
  (deref (.plot-style-atom data-mgmt)))


(defn data
  [^DataManagement data-mgmt]
  (dosync
    (ensure (.data-ref data-mgmt))))


(defn has-plot-data?
  [^DataManagement data-mgmt]
  (boolean (dosync (ensure (.data-ref data-mgmt)))))


(defn snapshots
  [^DataManagement data-mgmt]
  (dosync
    (let [snapshots (ensure (.snapshots-ref data-mgmt))]
      (when (seq snapshots)
        snapshots))))


(defn has-snapshots?
  [data-mgmt]
  (boolean (snapshots data-mgmt)))


(defn snapshot-by-id
  [^DataManagement data-mgmt, snapshot-id]
  (dosync
    (u/some-value
      #(-> % :id (= snapshot-id))
      (ensure (.snapshots-ref data-mgmt)))))


(defn snapshots-ref
  [^DataManagement data-mgmt]
  (.snapshots-ref data-mgmt))


(defn change-handlers
  [^DataManagement data-mgmt]
  (deref (.change-handlers-atom data-mgmt)))


(defn current-file
  ^File [^DataManagement data-mgmt]
  (deref (.current-file-atom data-mgmt)))


(defn add-change-handler
  "Adds a changer handler function to the data management for notification on changes.
  Change handler function have to accept three arguments: the data management instance, the new state and the previous state."
  [^DataManagement data-mgmt, handler-fn]
  (swap! (.change-handlers-atom data-mgmt) conj handler-fn)
  data-mgmt)


(defn notify-change-handlers
  "Notify all change handlers providing the new state and the previous state."
  [^DataManagement data-mgmt, plot-style, prev-state, new-state]
  (doseq [handler-fn (change-handlers data-mgmt)]
    (handler-fn data-mgmt, plot-style, prev-state, new-state)))


(defn group-set
  [state]
  (->> state
    :sample-group-map
    vals
    set))


(defn sample-groups-in-history+snapshots
  [^DataManagement data-mgmt]
  (->> (dosync
         (into (into [] (history data-mgmt))
           (mapv :data (snapshots data-mgmt))))
    (reduce
      (fn [group-set, {:keys [sample-group-map]}]
        (reduce-kv
          (fn [group-set, _, sample-group]
            (conj! group-set sample-group))
          group-set
          sample-group-map))
      (transient #{}))
    persistent!))


(defn update-plot-style-if-needed
  [^DataManagement data-mgmt, prev-state, {:keys [alteration-type-set] :as new-state},
   {:keys [sample-group->color, shown-alteration-type] :as plot-style}]
  (let [prev-group-set (group-set prev-state),
        new-group-set (group-set new-state),
        plot-style (cond-> plot-style
                     (and new-state (not (contains? alteration-type-set shown-alteration-type)))
                     (assoc :shown-alteration-type (some alteration-type-set [:mutation-data, :expression-data, :methylation-data])))]
    (if (or (nil? prev-state) (nil? new-state) (= prev-group-set new-group-set) (= (set (keys sample-group->color)) new-group-set))
      plot-style
      (let [all-sample-group-set (sample-groups-in-history+snapshots data-mgmt),
            ; remove all group color mappings that are not needed anymore (neither in history nor in snapshots)
            sample-group->color (reduce-kv
                                  (fn [sample-group->color, sample-group, _]
                                    (cond-> sample-group->color
                                      (not (contains? all-sample-group-set sample-group))
                                      (dissoc sample-group)))
                                  sample-group->color
                                  sample-group->color),
            assigned-groups (set (keys sample-group->color)),
            used-colors (->> sample-group->color vals set),
            available-colors (concat (remove used-colors kelly-colors) (cycle kelly-colors)),
            added-groups (vec (set/difference new-group-set prev-group-set assigned-groups)),
            new-plot-style (assoc plot-style
                             :sample-group->color (merge
                                                    sample-group->color
                                                    (zipmap added-groups available-colors)))]
        (reset! (.plot-style-atom data-mgmt) new-plot-style)
        (when-not (= new-plot-style plot-style)
          (dirty! data-mgmt))
        new-plot-style))))


(defn send-notification
  ([^DataManagement data-mgmt]
   (let [data (current-data data-mgmt)]
     (send-notification data-mgmt, data, data, (plot-style data-mgmt))))
  ([^DataManagement data-mgmt, prev-state, new-state]
   (send-notification data-mgmt, prev-state, new-state, (plot-style data-mgmt)))
  ([^DataManagement data-mgmt, prev-state, new-state, plot-style]
   (let [plot-style (update-plot-style-if-needed data-mgmt, prev-state, new-state, plot-style)]
     (send (.notification-agent data-mgmt)
       (fn [notification-count]
         (try
           (notify-change-handlers data-mgmt, prev-state, new-state, plot-style)
           (catch Throwable t
             (u/log-exception "Exception while notifying change handlers." t)))
         (inc notification-count)))
     data-mgmt)))


(defn pareto-front-data
  ([^DataManagement data-mgmt]
   (dosync (ensure (.pareto-front-data-ref data-mgmt))))
  ([^DataManagement data-mgmt, key-path]
   (-> (dosync (ensure (.pareto-front-data-ref data-mgmt)))
     (get-in (if (sequential? key-path) key-path [key-path])))))


(defn update-pareto-front-data
  [^DataManagement data-mgmt, f & args]
  (dosync
    (alter (.pareto-front-data-ref data-mgmt)
      #(apply f % args)))
  (doto data-mgmt
    dirty!
    send-notification))


(defn saved-pareto-fronts
  [data-mgmt]
  (pareto-front-data data-mgmt, [:single, :saved]))


(defn latest-pareto-front
  [^DataManagement data-mgmt]
  (pareto-front-data data-mgmt, [:single, :latest]))


(defn has-latest-pareto-front?
  [^DataManagement data-mgmt]
  (boolean (latest-pareto-front data-mgmt)))


(defn latest-pareto-front!
  [^DataManagement data-mgmt, pareto-front-data]
  (update-pareto-front-data data-mgmt, assoc-in [:single, :latest] pareto-front-data))


(defn plot-style!
  [^DataManagement data-mgmt, plot-style]
  (reset! (.plot-style-atom data-mgmt) plot-style)
  (dirty! data-mgmt)
  (let [data (current-data data-mgmt)]
    (send-notification data-mgmt, data, data, plot-style)))


(defn shown-alteration-type!
  [data-mgmt, alteration-type]
  (plot-style! data-mgmt
    (assoc (plot-style data-mgmt)
      :shown-alteration-type alteration-type)))


(defn shown-alteration-type
  [data-mgmt]
  (:shown-alteration-type (plot-style data-mgmt)))


(defn move-through-history
  [^DataManagement data-mgmt, ^long step]
  (dosync
    (let [{:keys [history, position]} (ensure (.data-ref data-mgmt)),
          new-position (+ position step),
          old-state (nth history position),
          new-state (nth history new-position)]
      (alter (.data-ref data-mgmt) assoc :position new-position)
      (dirty! data-mgmt)
      (send-notification data-mgmt, old-state, new-state)))
  data-mgmt)


(defn may-undo?
  [^DataManagement data-mgmt]
  ; use dosync as well, due to symmetry with may-redo?
  (dosync (< 0 (history-pos data-mgmt))))


(defn undo
  [^DataManagement data-mgmt]
  (dosync
    (when (may-undo? data-mgmt)
      (move-through-history data-mgmt, -1))))


(defn may-redo?
  [^DataManagement data-mgmt]
  (dosync
    (< (history-pos data-mgmt) (-> (history data-mgmt) count dec))))


(defn redo
  [^DataManagement data-mgmt]
  (dosync
    (when (may-redo? data-mgmt)
      (move-through-history data-mgmt, +1))))



(defn missing-gene-ids?
  [{:keys [gene-list] :as gene-sample-data}]
  (assert (some? gene-list) "gene sample data must have a gene list")
  (some #(not (contains? % :gene-id)) gene-list))

(defn ensure-gene-ids
  [{:keys [gene-list] :as gene-sample-data}]
  (cond-> gene-sample-data
    (missing-gene-ids? gene-sample-data)
    (assoc
      :gene-list
      (mapv
        (fn [id, gene]
          (assoc gene :gene-id id))
        (range)
        gene-list))))


(defn import-from-csv
  [^DataManagement data-mgmt, gene-sample-data]
  (dosync
    (alter (.data-ref data-mgmt) assoc
      :history (vector (ensure-gene-ids gene-sample-data))
      :position 0)
    (ref-set (.snapshots-ref data-mgmt) [])
    (ref-set (.pareto-front-data-ref data-mgmt) {:single {:saved [], :latest nil}, :batch []}))
  (reset! (.current-file-atom data-mgmt) nil)
  (reset! (.plot-style-atom data-mgmt) (assoc (default-plot-style)
                                         :shown-alteration-type
                                         (->> gene-sample-data
                                           :alteration-type-set
                                           gd/sort-alteration-types
                                           first)))
  (dirty! data-mgmt)
  (send-notification data-mgmt, nil, nil)
  (send-notification data-mgmt, nil, gene-sample-data))


(defn extinguish-future
  [history-vec, ^long n]
  (loop [i 0, history-vec history-vec]
    (if (< i n)
      (recur (unchecked-inc i), (pop history-vec))
      history-vec)))


(defn modify-current-data
  [^DataManagement data-mgmt, f, & args]
  (dosync
    (let [{:keys [history, position]} (ensure (.data-ref data-mgmt))
          old-state (when (seq history) (nth history position)),
          new-state (apply f old-state, args),
          history-size (count history)
          future-states-count (- history-size 1 position)]
      (alter (.data-ref data-mgmt)
        #(cond-> %
           (pos? future-states-count) (update-in [:history] extinguish-future future-states-count)
           ; only add to history if the state changed (e.g. reloading the same snapshot won't change the state and shouldn't increase the history size)
           (not= old-state new-state) (-> (update-in [:history] conj new-state) (update-in [:position] inc))))
      (dirty! data-mgmt)
      (send-notification data-mgmt, old-state, new-state)))
  data-mgmt)


(defn save-to-file
  [data-mgmt, file]
  (let [{:keys [data] :as file-data-map} (dosync
                                           {:data (data data-mgmt),
                                            :snapshots (snapshots data-mgmt),
                                            :pareto-front-data (pareto-front-data data-mgmt),
                                            :plot-style (plot-style data-mgmt)}),
        {:keys [history, position]} data]
    (ds/save-to-file file, file-data-map)
    (reset! (.current-file-atom ^DataManagement data-mgmt) (io/file file))
    (clean! data-mgmt)
    (let [data (when (seq history) (nth history position))]
      (send-notification data-mgmt, data, data))))


(defn load-from-file
  [^DataManagement data-mgmt, file]
  (let [file-data (ds/load-from-file file)]
    (if-let [error-map (schema/validate file-data)]
      ; file data format invalid
      (let [msg (format "Failed to load file \"%s\": invalid data format." (u/file-absolute-path file))]
        (u/log-error "%s\n%s" msg, error-map)
        (throw
          (ex-info
            (str msg "\nDid you try to load a file that has been saved with a newer version of this software?")
            {:scope :load-from-file, :action :format-validation, :file file, :error-map error-map})))
      ; file data format valid
      (let [{:keys [data, snapshots, pareto-front-data, plot-style]} file-data
            ; overwrite stored settings in the default plot style (this way new style settings are properly initialized)
            plot-style (merge (default-plot-style) plot-style)
            {:keys [history, position]} data]
        (dosync
          (ref-set (.data-ref data-mgmt) data)
          (ref-set (.snapshots-ref data-mgmt) snapshots)
          (ref-set (.pareto-front-data-ref data-mgmt) pareto-front-data))
        (reset! (.plot-style-atom data-mgmt) plot-style)
        (reset! (.current-file-atom data-mgmt) (io/file file))
        (clean! data-mgmt)
        (send-notification data-mgmt, nil, nil)
        (send-notification data-mgmt, nil, (when (seq history) (nth history position)), plot-style)))))


(defn add-snapshot
  [snapshots-vec, snapshot]
  (let [max-id (transduce (map :id) max 0 snapshots-vec)]
    (conj (or snapshots-vec []) (assoc snapshot :id (inc max-id)))))


(defn save-snapshot
  ([^DataManagement data-mgmt, snapshot-name]
   (save-snapshot data-mgmt, snapshot-name, (current-data data-mgmt), (shown-alteration-type data-mgmt)))
  ([^DataManagement data-mgmt, snapshot-name, plot-data, shown-alteration-type]
   (let [timestamp (u/current-timestamp)]
     (doto data-mgmt
       (->
         .snapshots-ref
         (alter add-snapshot {:name snapshot-name, :created timestamp, :data plot-data, :shown-alteration-type shown-alteration-type})
         dosync)
       dirty!
       send-notification))))


(defn load-snapshot
  [^DataManagement data-mgmt, snapshot-id]
  (dosync
    (when-let [selected-snapshot (->> data-mgmt snapshots (filter #(= (:id %) snapshot-id)) first)]
      (modify-current-data data-mgmt, (constantly (:data selected-snapshot)))
      (shown-alteration-type! data-mgmt, (:shown-alteration-type selected-snapshot)))))


(defn delete-snapshot
  [^DataManagement data-mgmt, snapshot-id]
  (doto data-mgmt
    (->
      .snapshots-ref
      (alter (fn [snapshot-vec]
               (when-let [snapshot-position (u/some-index #(-> % :id (= snapshot-id)) snapshot-vec)]
                 (vec
                   (concat
                     (subvec snapshot-vec 0, snapshot-position)
                     (subvec snapshot-vec (inc snapshot-position)))))))
      dosync)
    dirty!
    send-notification))


(defn save-pareto-front
  [^DataManagement data-mgmt, pareto-front-name, pareto-front-data]
  (let [record {:name pareto-front-name, :data pareto-front-data}]
    (update-pareto-front-data data-mgmt, update-in [:single, :saved] conj record)))


(defn save-batch
  [^DataManagement data-mgmt, batch]
  (update-pareto-front-data data-mgmt, update-in [:batch] conj batch))


(defn has-saved-pareto-fronts?
  [^DataManagement data-mgmt]
  (boolean (seq (pareto-front-data data-mgmt, [:single, :saved]))))


(defn has-batch-pareto-fronts?
  [^DataManagement data-mgmt]
  (boolean (seq (pareto-front-data data-mgmt, [:batch]))))


(defn pareto-front-data-ref
  [^DataManagement data-mgmt]
  (.pareto-front-data-ref data-mgmt))


(defn saved-pareto-front
  [^DataManagement data-mgmt, pareto-front-position]
  (pareto-front-data data-mgmt, [:single, :saved, pareto-front-position]))


(defn delete-saved-pareto-fronts
  [^DataManagement data-mgmt, pareto-front-position-set]
  (update-pareto-front-data data-mgmt, update-in [:single, :saved] (partial u/delete pareto-front-position-set)))


(defn batch-pareto-front
  [data-mgmt, batch-index, front-index]
  (pareto-front-data data-mgmt, [:batch, batch-index, :pareto-front-list, front-index]))


(defn pareto-front-batches
  [data-mgmt]
  (pareto-front-data data-mgmt, [:batch]))


(defn delete-batch-pareto-fronts
  [data-mgmt, batch-index, front-index-set]
  (update-pareto-front-data data-mgmt, update-in [:batch]
    (fn [batch-vec]
      (let [batch (-> batch-vec
                    (nth batch-index)
                    (update-in [:pareto-front-list] (partial u/delete front-index-set)))]
        ; is therere at least one pareto front left?
        (if (seq (:pareto-front-list batch))
          ; store batch with remaining pareto fronts
          (assoc batch-vec batch-index batch)
          ; delete empty batch
          (u/delete #{batch-index} batch-vec))))))