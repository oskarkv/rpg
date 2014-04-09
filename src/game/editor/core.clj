(ns game.editor.core
  (:import (com.jme3.system AppSettings JmeCanvasContext)
           (com.jme3.app FlyCamAppState)
           (java.util LinkedList))
  (:require (game [game-map :as game-map])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [graphics :as gfx]
                         [input :as cmn-input])
            (game.editor [input :as e-input]
                         [editor-functions :as efns])
            (game.key-value-store [protocols :as kvs])))

(defmulti process-tap (fn [_ type] type))

(defmethod process-tap :perform-selected-action [game-state type]
  (if (:current-action game-state)
    (let [{:keys [new-game-state events]}
          ((:current-action game-state) game-state)]
      {:new-game-state new-game-state :events events})))

(defn process-player-input [game-state key-state]
  (let [{:keys [new-game-state events]}
        (ccfns/process-player-input game-state key-state process-tap)]
    {:new-game-state new-game-state :events events}))

(def event-queue (LinkedList.))

(defn enqueue-event [event]
  (.add event-queue event))

(defmulti process-event (fn [_ event] (event :type)))

(defmethod process-event :save-zone [args-map event]
  (efns/save-zone (:game-state args-map) (:key-value-store args-map))
  args-map)

(defn- process-event-queue [args-map]
  (reduce process-event args-map event-queue))

(defn process-event-caller [game-state key-value-store]
  (let [args-map {:game-state game-state :key-value-store key-value-store}
        new-args-map (process-event-queue args-map)]
    {:game-state (:game-state new-args-map)
     :events (:events new-args-map)}))


(defn create-editor-jme3-app [game-state-atom key-value-store]
  (let [stop? (atom false)
        key-bindings (e-input/load-key-bindings)
        key-state-atom (atom (cmn-input/create-key-state-map key-bindings))
        graphics-system (atom nil)
        start-input-fn
        (fn [input-manager]
          (cmn-input/start-input
            input-manager
            key-bindings
            key-state-atom))
        init-gfx-fn
        (fn [app]
          (gfx/init-graphics-system
            app
            (:game-map @game-state-atom)))
        simple-init-fn
        (fn [app]
          (let [state-manager (.getStateManager app)
                input-manager (.getInputManager app)
                asset-manager (.getAssetManager app)
                root-node (.getRootNode app)]
            (.detach state-manager (.getState state-manager FlyCamAppState))
            (.setCursorVisible input-manager true)
            (start-input-fn input-manager)
            (reset! graphics-system (init-gfx-fn app))
            (cc/start @graphics-system)))
        simple-update-fn
        (fn []
          (Thread/sleep 50)
          (let [{:keys [new-game-state events]}
                (ccfns/call-update-fns @game-state-atom [] nil
                  (process-player-input @key-state-atom)
                  (process-event-caller key-value-store))]
            (cmn-input/empty-taps key-state-atom)
            (.clear event-queue)
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
