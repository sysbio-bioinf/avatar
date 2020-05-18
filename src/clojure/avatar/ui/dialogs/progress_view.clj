; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.dialogs.progress-view
  (:refer-clojure :exclude [update])
  (:require
    [clj-jfx.core :as jfx]
    [avatar.algorithms.progress-tracker :as progress]
    [avatar.util :as u]
    [clojure.stacktrace :as st]
    [avatar.ui.tools :as t])
  (:import
    (javafx.scene.control Label ProgressBar)
    (javafx.stage WindowEvent)))




(deftype SingleProgressTracker [state-atom]
  progress/ProgressTracker

  (start [this]
    (u/safe
      (let [now (System/currentTimeMillis)]
        (swap! state-atom assoc
          :start-time now
          :update-time now))))

  (update [this]
    (progress/update this, 1))

  (update [this, completed-steps]
    (u/safe-future
      (swap! state-atom
        #(-> %
           (update-in [:completed-tasks] + completed-steps)
           (update-in [:update-time] max (System/currentTimeMillis))))))

  (finished [this]
    (u/safe-future
      (swap! state-atom
        (fn [{:keys [total-tasks] :as state}]
          (assoc state :completed-tasks total-tasks))))))



(defn setup-progress-bar
  [^ProgressBar progress-bar, completed-tasks-prop, total-tasks-prop]
  (jfx/bind (jfx/property progress-bar :progress)
    (jfx/functional-property
      (fn [completed-tasks, total-tasks]
        (if (and completed-tasks total-tasks)
          (/ (double completed-tasks) total-tasks)
          0.0))
      completed-tasks-prop
      total-tasks-prop)))


(defn setup-duration-label
  [^Label duration-label, completed-tasks-prop, total-tasks-prop, start-time-prop, update-time-prop]
  (jfx/bind (jfx/property duration-label, :text),
    (jfx/functional-property
      (fn [completed-tasks, total-tasks, start-time, update-time]
        (if-let [duration (progress/remaining-duration completed-tasks, total-tasks, start-time, update-time)]
          (progress/duration-str duration)
          "N/A"))
      completed-tasks-prop
      total-tasks-prop
      start-time-prop
      update-time-prop)))


(defn show-progress
  [task-name, total-task-count]
  (jfx/run-now
    (let [progress-atom (atom {:completed-tasks 0,
                               :total-tasks total-task-count,
                               :start-time nil,
                               :update-time nil}),
          control (jfx/create-control "avatar/ui/ProgressView.fxml"),
          control-node (jfx/control-node control),
          {:keys [^Label caption-label,
                  ^Label duration-label,
                  progress-bar]} (jfx/control-children control),
          window (doto (jfx/modal-window "Progress", control-node)
                   (jfx/window-style! :utility)
                   (t/add-avatar-icon)
                   (jfx/handle-event! :window-close-request
                     (fn [^WindowEvent e]
                       (let [{:keys [completed-tasks, total-tasks]} (deref progress-atom)]
                         (when-not (= completed-tasks total-tasks)
                           (.consume e))))))
          completed-tasks-prop (jfx/map-entry-property progress-atom, [:completed-tasks])
          total-tasks-prop (jfx/map-entry-property progress-atom, [:total-tasks])]

      (jfx/property-value! caption-label, :text task-name)

      ; progress bar
      (setup-progress-bar progress-bar, completed-tasks-prop, total-tasks-prop)

      ; estimated duration
      (setup-duration-label duration-label, completed-tasks-prop, total-tasks-prop,
        (jfx/map-entry-property progress-atom, [:start-time])
        (jfx/map-entry-property progress-atom, [:update-time]))

      (jfx/listen-to
        (fn [completed-tasks, total-tasks]
          (when (and completed-tasks total-tasks (== completed-tasks total-tasks))
            (jfx/close window)))
        completed-tasks-prop
        total-tasks-prop)

      (jfx/show window)
      (SingleProgressTracker. progress-atom))))


(defn create-progress-control
  [state-atom, index, {:keys [name]}]
  (let [control (jfx/create-control "avatar/ui/ProgressElement.fxml")
        control-node (jfx/control-node control)
        {:keys [caption-vbox,
                ^Label duration-label,
                ^ProgressBar progress-bar]} (jfx/control-children control)
        completed-tasks-prop (jfx/map-entry-property state-atom, [index, :completed-tasks])
        total-tasks-prop (jfx/map-entry-property state-atom, [index, :total-tasks])]

    ; name label
    (jfx/add-children caption-vbox,
      (u/mapv-indexed
        (fn [index, name-part]
          (doto (Label. name-part)
            (cond-> (zero? index) (jfx/text-font! {:weight :bold}))
            (jfx/max-width! 300)))
        (cond-> name (not (sequential? name)) vector)))

    ; progress bar
    (setup-progress-bar
      progress-bar
      (jfx/map-entry-property state-atom, [index, :completed-tasks])
      (jfx/map-entry-property state-atom, [index, :total-tasks]))

    ; estimated duration
    (setup-duration-label duration-label, completed-tasks-prop, total-tasks-prop,
      (jfx/map-entry-property state-atom, [index, :start-time])
      (jfx/map-entry-property state-atom, [index, :update-time]))

    (jfx/grow-always-in-vbox! control-node)))



(deftype MultipleProgressTrackerIndividual [state-atom, index]
  progress/ProgressTracker

  (start [this]
    (u/safe
      (let [now (System/currentTimeMillis)]
        (swap! state-atom update-in [index] assoc
          :start-time now
          :update-time now))))

  (update [this]
    (progress/update this, 1))

  (update [this, completed-steps]
    (u/safe
      (swap! state-atom update-in [index]
        #(-> %
           (update-in [:completed-tasks] + completed-steps)
           (update-in [:update-time] max (System/currentTimeMillis))))))

  (finished [this]
    (u/safe
      (swap! state-atom update-in [index]
        (fn [{:keys [total-tasks] :as state}]
          (assoc state :completed-tasks total-tasks))))))


(deftype MultipleProgressTracker [state-atom, individual-progress-trackers]
  progress/MultipleProgressTracker
  (individual-progress-tracker [this, index]
    (nth individual-progress-trackers index)))


(defn show-multiple-progresses
  [caption, process-info-list]
  (jfx/run-now
    (let [state-atom (atom
                       (mapv
                         (fn [{:keys [name, total-tasks, difficulty]}]
                           {:completed-tasks 0,
                            :total-tasks total-tasks,
                            :start-time nil,
                            :update-time nil,
                            :name name,
                            :difficulty difficulty})
                         process-info-list)),
          process-controls (u/mapv-indexed
                             (partial create-progress-control state-atom)
                             process-info-list)
          control (jfx/create-control "avatar/ui/MultipleProgressTracker.fxml"),
          control-node (jfx/control-node control),
          {:keys [^Label caption-label
                  ^Label total-duration-label
                  content-vbox]} (jfx/control-children control)
          everything-done? (fn [process-list]
                             (every?
                               (fn [{:keys [completed-tasks, total-tasks]}]
                                 (= completed-tasks total-tasks))
                               process-list))
          window (doto (jfx/modal-window "Progress", control-node)
                   (jfx/window-style! :utility)
                   (t/add-avatar-icon)
                   (jfx/handle-event! :window-close-request
                     (fn [^WindowEvent e]
                       (when-not (everything-done? (deref state-atom))
                         (.consume e)))))

          estimated-duration-prop (jfx/object-property (progress/estimate-total-duration (deref state-atom)))]

      (add-watch state-atom :estimate-duration
        (fn [_, _, old-state, new-state]
          (when-not (= old-state new-state)
            (jfx/run-now (jfx/value! estimated-duration-prop, (progress/estimate-total-duration new-state)))
            (when (everything-done? new-state)
              (jfx/close window)))))

      (jfx/property-value! caption-label, :text, caption)
      (jfx/add-children content-vbox, process-controls)

      (jfx/bind (jfx/property total-duration-label, :text), estimated-duration-prop)

      (jfx/show window)
      (MultipleProgressTracker.
        state-atom,
        (mapv #(MultipleProgressTrackerIndividual. state-atom, %) (range (count process-info-list)))))))


(defn show-indeterminate-progress
  [title]
  (jfx/run-now
    (let [finished?-atom (atom false)
          control (jfx/create-control "avatar/ui/IndeterminateProgressView.fxml"),
          control-node (jfx/control-node control),
          {:keys [^Label title-label]} (jfx/control-children control)
          window (doto (jfx/modal-window "Progress", control-node)
                   (jfx/window-style! :utility)
                   (t/add-avatar-icon)
                   (jfx/handle-event! :window-close-request
                     (fn [^WindowEvent e]
                       (when-not (deref finished?-atom)
                         (.consume e)))))]
      (jfx/property-value! title-label, :text, title)
      (add-watch finished?-atom, :finished?,
        (fn [_, _, _, finished?]
          (when finished?
            (jfx/close window))))
      (jfx/show window)
      finished?-atom)))


(defn indeterminate-progress-finished
  [finished?-atom]
  (u/safe-future (reset! finished?-atom true)))


(defmacro with-shown-indeterminate-progress
  [title, & body]
  `(let [progress-state# (show-indeterminate-progress ~title)]
     (try
       ~@body
       (finally
         (indeterminate-progress-finished progress-state#)))))