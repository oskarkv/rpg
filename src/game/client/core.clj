(ns game.client.core
  (:require
   [clojure.set :as set]
   [game.client.graphics :as gfx]
   [game.client.hud :as hud]
   [game.client.input :as input]
   [game.client.base :as b]
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns]
   [game.common.spells :as csp]
   [game.constants :as consts]
   [game.game-map :as gmap]
   [game.client.inventory :as inv]
   [game.items :as items]
   [game.math :as math]
   [game.networking.core :as net]
   [game.utils :refer :all])
  (:import
   (com.jme3.app FlyCamAppState SimpleApplication)
   (com.jme3.app.state AbstractAppState)
   (com.jme3.system AppSettings)))

(defmethod b/process-event :s-own-id [game-state {id :id}]
  (assoc game-state :own-id id))

(defn process-received-game-state [game-state]
  (let [curr-time (current-time-ms)]
    (assoc game-state :last-move curr-time)))

(defmethod b/process-event :s-game-state [game-state {new-game-state :game-state}]
  (merge game-state (process-received-game-state new-game-state)))

(defmethod b/process-event :s-move-chars [game-state {:keys [positions]}]
  (reduce (fn [gs [id pos]] (assoc-in gs [:chars id :new-pos] pos))
          game-state
          (dissoc positions (:own-id game-state))))

(defmethod b/process-event :s-char-death [game-state {:keys [id]}]
  (if (= (:own-id game-state) id)
    (update-in game-state [:chars id] dissoc :target :attacking)
    (dissoc-in game-state [:chars id])))

(defmethod b/process-event :s-spawn-corpse [game-state {:keys [id corpse]}]
  (assoc-in game-state [:corpses id] corpse))

(defmethod b/process-event :s-spawn-player [game-state {:keys [id player]}]
  (update-in game-state [:chars id] merge player))

(defmethod b/process-event :s-attack
  [game-state {:keys [target damage hit] :as event}]
  (when hit
    (update-in game-state [:chars target :hp] - damage)))

(defmethod b/process-event :s-spawn-mobs [game-state {:keys [mobs]}]
  (update game-state :chars merge mobs))

(defmethod b/process-event :s-decay-corpses [game-state {:keys [ids]}]
  (-> game-state
    (update :corpses #(apply dissoc % ids))
    (update :looting #(apply disj % ids))))

(defmethod b/process-event :s-loot [game-state {:keys [corpse-id drops]}]
  (-> game-state
    (assoc-in [:corpses corpse-id :drops] drops)
    (update :looting conj corpse-id)))

(defmethod b/process-event :s-regen-tick [game-state {:keys [update-map]}]
  (update game-state :chars (partial merge-with merge) update-map))

(defmethod b/process-event :s-heal [game-state {:keys [target by amount]}]
  ;; The server makes sure the amount does not make the :hp go over :max-hp
  (update-in game-state [:chars target :hp] + amount))

(defmethod b/process-event :s-char-update [game-state {:keys [id updated]}]
  (let [new-level (:level updated)
        old-level (get-in game-state [:chars id :level])
        leveled-up (> new-level old-level)]
    (when leveled-up
      (b/enqueue-events {:type :level-up :id id}))
    (cond-> (update-in game-state [:chars id] merge updated)
      (== (:own-id game-state) id) inv/update-own-stats)))

(defmethod b/process-event :level-up [game-state {:keys [id]}]
  (println "DING!"))

(defn calculate-base-movement-direction [key-state]
  (let [adder (fn [dx dy] (fn [[x y]] [(+ dx x) (+ dy y)]))
        {:keys [forward back left right]} key-state]
    (cond-> [0 0]
      forward ((adder 0 1))
      back ((adder 0 -1))
      left ((adder -1 0))
      right ((adder 1 0))
      true (math/normalize))))

(defmethod b/process-event :new-key-state [game-state {:keys [key-state]}]
  (let [new-base-dir (map float (calculate-base-movement-direction key-state))
        modifiers (select-keys key-state [:shift :alt :ctrl])
        new-game-state (assoc game-state
                              :base-move-dir new-base-dir
                              :modifiers modifiers)]
    new-game-state))

(defmethod b/process-event :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:chars own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    (b/enqueue-events {:type :c-move :pos pos :move-dir dir})))

(defmethod b/process-event :toggle-attack [{id :own-id :as game-state} _]
  (b/enqueue-events {:type :c-toggle-attack})
  (update-in game-state [:chars id :attacking] not))

(defn new-target [game-state]
  (let [id (:own-id game-state)
        target (gfx/pick-target :chars)]
    (when target
      (b/enqueue-events {:type :c-target :target target})
      (assoc-in game-state [:chars id :target] target))))

(defmethod b/process-event :left-click [game-state _]
  (if (:on-mouse game-state)
    (inv/init-destroy-item game-state)
    (new-target game-state)))

(defmethod b/process-event :right-click [game-state _]
  (when-let [corpse (gfx/pick-target :corpses)]
    (when (ccfns/id-close-enough? game-state (:own-id game-state)
                                  corpse consts/loot-distance)
      (b/enqueue-events {:type :c-loot-corpse :corpse-id corpse}))))

(defmethod b/process-event :spell
  [{:keys [chars own-id] :as game-state} {:keys [number]}]
  (when-lets [target-id (get-in chars [own-id :target])
              _ (chars target-id)
              {:keys [spell last-cast]} (get-in game-state [:spells number])
              cd (get-in csp/spells [spell :cooldown])
              curr-time (current-time-ms)
              _ (> curr-time (+ last-cast (* 1000 cd)))]
    (b/enqueue-events {:type :c-cast-spell :number number
                       :target target-id})
    (assoc-in game-state [:spells number :last-cast] (+ curr-time 1e6))))

(defmethod b/process-event :s-spell-response [game-state event]
  (let [{:keys [response number]} event]
    (assoc-in game-state [:spells number :last-cast]
              (case response
                :out-of-range (do (println "Out of range!") 0)
                :cooldown (do (println "Cooldown!") 0)
                :out-of-mana (do (println "Out of mana!") 0)
                :ok (current-time-ms)))))

(defmethod b/process-event :s-spell-cast [game-state {:keys [by spell mana-cost]}]
  (update-in game-state [:chars by :mana] - mana-cost))

(defn legal-pos? [{:keys [terrain] :as game-state} pos]
  (let [r consts/player-radius
        -r (- r)
        adder (fn [v] (map + pos v))]
    (every? gmap/traversable?
            (map #(get-in terrain (mapv (comp int math/floor) %))
                 (map adder [[r r] [r -r] [-r r] [-r -r]])))))

(defn update-looting [game-state new-pos]
  (let [get-pos #(get-in game-state [:corpses % :pos])]
    (set (filter #(ccfns/pos-close-enough? (get-pos %) new-pos
                                           consts/loot-distance)
                 (:looting game-state)))))

(defn move-self [{:keys [looting own-id chars move-time-delta] :as game-state}]
  (let [{:keys [pos move-dir speed] :as self} (chars own-id)
        new-pos (math/extrapolate-pos pos move-dir move-time-delta speed)]
    (if (legal-pos? game-state new-pos)
      (let [new-looting (update-looting game-state new-pos)
            ngs (-> game-state
                  (assoc-in [:chars own-id :pos] new-pos)
                  (assoc :looting new-looting))
            quitted (set/difference looting new-looting)]
        (when (seq quitted)
          (b/enqueue-events {:type :c-quit-looting :ids quitted}))
        ngs)
      (do
        (b/enqueue-events {:type :new-dir})
        (assoc-in game-state [:chars own-id :move-dir] [0 0])))))

(defn move-toward-new-pos [{:keys [pos new-pos speed] :as char} time-delta _]
  (if new-pos
    (let [new-char (ccfns/move-toward-pos char time-delta new-pos)]
      (if (= (:pos new-char) new-pos)
        (dissoc new-char :new-pos)
        new-char))
    char))

(defn move-chars [game-state]
  (let [{:keys [chars move-time-delta last-move]} game-state]
    (assoc-in game-state [:chars]
              (fmap #(move-toward-new-pos % move-time-delta last-move)
                    chars))))

(defn update-looking-direction [{:keys [last-dir-update] :as game-state}]
  (when (> (current-time-ms) (+ last-dir-update consts/dir-update-interval))
    (-> game-state
      (assoc :looking-dir (gfx/get-camera-dir))
      (assoc :last-dir-update (current-time-ms)))))

(defn calculate-movement-direction
  [{:keys [base-move-dir own-id looking-dir] :as game-state}]
  (let [angle (math/angle-between [0 1] looking-dir)
        new-move-dir (map float (math/rotate-vec base-move-dir angle))
        old-move-dir (map float (get-in game-state [:chars own-id :move-dir]))]
    (when-not (rec== new-move-dir old-move-dir)
      (b/enqueue-events {:type :new-dir})
      (assoc-in game-state [:chars own-id :move-dir] new-move-dir))))

(defn initial-game-state []
  (-> {} (assoc :base-move-dir [0 0]) (assoc :last-dir-update 0)
      (assoc :looting #{})))

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
                  (update-looking-direction)
                  (calculate-movement-direction)
                  (move-self)
                  (move-chars))
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
  (let [game-state-atom (atom (initial-game-state))
        networking-system (atom (net/init-client-net-sys address port))
        [stop? app]
        (create-client-jme3-app networking-system game-state-atom)]
    [stop? (->Client app)]))
