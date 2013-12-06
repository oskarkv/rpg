(ns game.editor.core
  (:import (com.jme3.system AppSettings JmeCanvasContext)
           (com.jme3.app FlyCamAppState))
  (:require (game [game-map :as game-map])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [graphics :as gfx]
                         [input :as cmn-input])
            (game.editor [gui :as gui]
                         [input :as e-input])))

(defmulti process-tap (fn [_ type] type))

(defmethod process-tap :perform-selected-action [editor-state type]
  (if (:current-action editor-state)
    (let [{:keys [editor-state events]}
          ((:current-action editor-state) editor-state)]
      {:editor-state editor-state :events events})
    {:editor-state editor-state}))

(defn process-player-input [editor-state key-state]
  (let [{:keys [new-app-state events]}
        (ccfns/process-player-input editor-state key-state process-tap)]
    {:new-editor-state new-app-state :events events}))

(defrecord Editor [app j-frame]
  cc/Lifecycle
  (start [this]
    (cc/start app)
    (cc/start j-frame)
    this)
  (stop [this]
    (cc/stop app)
    this))

(defn create-editor-jme3-app [editor-state-atom]
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
            (:game-map @editor-state-atom)))
        simple-init-fn
        (fn [app]
          (let [state-manager (.getStateManager app)
                input-manager (.getInputManager app)
                asset-manager (.getAssetManager app)
                root-node (.getRootNode app)]
            (.detach state-manager (.getState state-manager
                                              FlyCamAppState))
            (.setCursorVisible input-manager true)
            (start-input-fn input-manager)
            (reset! graphics-system (init-gfx-fn app))
            (cc/start @graphics-system)))
        simple-update-fn
        (fn []
          (Thread/sleep 10)
          (ccfns/call-update-fns @editor-state-atom []
            (process-player-input @key-state-atom))
          (cmn-input/empty-taps key-state-atom))
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

(defn create-editor-swing-app [app editor-state-atom]
  (let [j-frame (gui/create-editor-swing-app app editor-state-atom)]
    j-frame))

(defn init-editor []
  (let [game-map (game-map/load-game-map)
        editor-state-atom (atom {:game-map game-map})
        app (create-editor-jme3-app editor-state-atom)
        j-frame (create-editor-swing-app app editor-state-atom)]
    (->Editor app j-frame)))
