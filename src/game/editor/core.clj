(ns game.editor.core
  (:require
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns]
   [game.common.graphics :as gfx]
   [game.common.input :as cmn-input]
   [game.editor.editor-functions :as efns]
   [game.editor.gui :as gui]
   [game.editor.input :as e-input])
  (:import
   (com.jme3.app FlyCamAppState)
   (com.jme3.system AppSettings)))

(defmulti process-event (fn [game-state event args-map] (event :type)))

(defmethod process-event :perform-selected-action [game-state event args-map]
  (if (:current-action game-state)
    (let [{:keys [new-game-state event]}
          ((:current-action game-state) game-state)]
      {:new-game-state new-game-state :event event})))

(defmethod process-event :cancel-action [game-state event args-map]
  (if (:current-action game-state)
    (let [{:keys [new-game-state event]}
          (efns/cancel-action game-state)]
      {:new-game-state new-game-state :event event})))

(defmethod process-event :edit-target [game-state event args-map]
  (when-let [target (gfx/pick-target :spawns)]
    ))

(defmethod process-event :save-zone [game-state event args-map]
  (efns/save-zone game-state (:key-value-store args-map))
  game-state)

(defmethod process-event :load-zone [game-state event args-map]
  (let [new-game-state
        (efns/load-zone game-state (:key-value-store args-map))]
    new-game-state))

(defmethod process-event :edit-zone [game-state event args-map]
  (gui/edit-zone))

(defn- process-event-queue [game-state event-queue args-map]
  (reduce process-event args-map event-queue))

(defn process-event* [args-map]
  (fn [game-state event]
    (process-event game-state event args-map)))

(defn process-events [game-state event-queue key-value-store]
  (let [args-map {:key-value-store key-value-store}]
    (ccfns/process-events (process-event* args-map) game-state event-queue)))

(defn get-subsystem-events [_ systems]
  {:events (mapcat cc/get-events systems)})

(defn create-editor-jme3-app [game-state-atom event-queue key-value-store]
  (let [stop? (atom false)
        key-bindings (e-input/load-key-bindings)
        key-state-atom (atom (cmn-input/create-key-state-map key-bindings))
        input-system (atom nil)
        graphics-system (atom nil)
        get-subsystems (fn [] [@graphics-system @input-system])
        init-gfx-fn
        (fn [app]
          (gfx/init-graphics-system app (:terrain (:game-map @game-state-atom))))
        simple-init-fn
        (fn [app]
          (let [state-manager (.getStateManager app)
                input-manager (.getInputManager app)
                asset-manager (.getAssetManager app)
                root-node (.getRootNode app)]
            (.detach state-manager (.getState state-manager FlyCamAppState))
            (.setCursorVisible input-manager true)
            (reset! input-system (cmn-input/init-input-system
                                  input-manager (e-input/load-key-bindings)))
            (reset! graphics-system (init-gfx-fn app))
            (dorun (map cc/start (get-subsystems)))))
        simple-update-fn
        (fn []
          (Thread/sleep 100)
          (let [{:keys [new-game-state events]}
                (ccfns/call-update-fns @game-state-atom [] nil
                  (get-subsystem-events (get-subsystems)))
                {:keys [new-game-state]}
                (process-events new-game-state event-queue key-value-store)]
            (.clear event-queue)
            (cc/update @graphics-system new-game-state)
            (reset! game-state-atom new-game-state)))
        init-app-settings-fn
        (fn [app]
          (doto app
            (.setShowSettings false)
            (.setSettings (AppSettings. true))
            (.setPauseOnLostFocus false)
            (.createCanvas)))
        start-fn
        (fn [this]
          (.startCanvas this))
        stop-fn
        (fn [this]
          (reset! stop? true)
          (cc/stop @graphics-system)
          (.stop this))
        app (ccfns/create-jme3-app
             start-fn stop-fn
             simple-init-fn simple-update-fn
             init-app-settings-fn)]
    app))
