(ns game.client.core
  (:require
   [game.client.base :as b]
   [game.client.gameplay :as gp]
   [game.client.graphics :as gfx]
   [game.client.hud :as hud]
   [game.client.input :as input]
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns]
   [game.constants :as consts]
   [game.networking.core :as net]
   [game.utils :refer :all])
  (:import
   (com.jme3.app FlyCamAppState SimpleApplication)
   (com.jme3.app.state AbstractAppState)
   (com.jme3.system AppSettings)))

(defn get-subsystem-events [_ systems]
  (apply b/enqueue-events (mapcat cc/get-events systems)))

(def process-events (ccfns/make-process-events b/process-event))

(defn make-process-and-send-fn [networking-system]
  (ccfns/make-process-event-queue
   (fn [game-state events]
     (cc/update networking-system events)
     (process-events game-state events))
   b/event-queue))

(defn login-and-recv-state [game-state net-sys name password stop?]
  (letfn [(move-out [gs k]
            (move-in gs [:chars (:own-id gs) k] [k]))
          (move-out-special-maps [gs]
            (-> gs (move-out :inv) (move-out :gear) (move-out :spells)))]
    (cc/update net-sys [{:type :c-login :username name :password password}])
    (loop [events (cc/get-events net-sys)]
      (when-not @stop?
        (if (-> (fn [e] (contains? #{:s-game-state :s-own-id} (:type e)))
              (filter events) count (== 2))
          (move-out-special-maps (process-events game-state events))
          (recur (concat events (cc/get-events net-sys))))))))

(defrecord Client [app]
  cc/Lifecycle
  (start [this]
    (cc/start app)
    this)
  (stop [this]
    (cc/stop app)
    this))

(defn create-jme3-app [start-fn stop-fn init-fn update-fn init-app-settings-fn]
  (let [app
        (init-app-settings-fn
         (proxy [SimpleApplication] []
           (simpleInitApp []
             (init-fn this))
           (simpleUpdate [tpf]
             (update-fn))))]
    (extend-type (type app)
      cc/Lifecycle
      (start [this]
        (start-fn this))
      (stop [this]
        (stop-fn this)))
    app))

(defn create-client-jme3-app [networking-system game-state-atom]
  (let [stop? (atom false)
        graphics-system (atom nil)
        hud-system (atom nil)
        input-system (atom nil)
        get-subsystems (fn [] [@networking-system @graphics-system
                               @hud-system @input-system])
        simple-init-fn
        (fn [app]
          (let [state-manager (.getStateManager app)
                input-manager (.getInputManager app)
                asset-manager (.getAssetManager app)
                root-node (.getRootNode app)]
            (.detach state-manager (.getState state-manager FlyCamAppState))
            (.attach state-manager (proxy [AbstractAppState] []
                                     (cleanup []
                                       (reset! stop? true))))
            (.setCursorVisible input-manager true)
            (cc/start @networking-system)
            (reset! game-state-atom
                    (login-and-recv-state @game-state-atom @networking-system
                                          "leif" "star" stop?))
            (reset! graphics-system
                    (gfx/init-graphics-system app (:terrain @game-state-atom)))
            (reset! hud-system
                    (hud/init-hud-system app))
            (reset! input-system
                    (input/init-input-system
                     input-manager (input/load-key-bindings)))
            (dorun (map cc/start (remove #{@networking-system}
                                         (get-subsystems))))
            (gfx/set-up-camera @graphics-system @game-state-atom)))
        simple-update-fn
        (fn []
          (Thread/sleep 1)
          (let [hook (make-process-and-send-fn @networking-system)
                new-game-state
                (ccfns/call-update-fns @game-state-atom hook
                  (get-subsystem-events (get-subsystems))
                  (ccfns/calculate-move-time-delta)
                  (gp/update-looking-direction)
                  (gp/calculate-movement-direction)
                  (gp/move-self)
                  (gp/move-chars))
                events (ccfns/reset-queue b/all-events-queue)]
            (cc/update @graphics-system {:game-state new-game-state
                                         :events events})
            (cc/update @hud-system {:game-state new-game-state :events events})
            (reset! game-state-atom new-game-state)))
        init-app-settings-fn
        (fn [app]
          (doto app
            (.setShowSettings false)
            (.setSettings
             (doto (AppSettings. true)
               (.setResolution consts/resolution-x consts/resolution-y)))
            (.setPauseOnLostFocus false)))
        start-fn
        (fn [this]
          (.start this))
        stop-fn
        (fn [this]
          (reset! stop? true)
          (runmap cc/stop (get-subsystems))
          (.stop this))]
    [stop? (create-jme3-app start-fn stop-fn
                            simple-init-fn simple-update-fn
                            init-app-settings-fn)]))

(defn init-client [address port headless]
  (let [game-state-atom (atom (gp/initial-game-state))
        networking-system (atom (net/init-client-net-sys address port))
        [stop? app]
        (create-client-jme3-app networking-system game-state-atom)]
    [stop? (->Client app)]))
