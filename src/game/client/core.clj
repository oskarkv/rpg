(ns game.client.core
  (:import (com.jme3.system AppSettings JmeContext$Type)
           (com.jme3.app FlyCamAppState)
           (com.jme3.app.state AbstractAppState))
  (:require [game.networking.core :as net]
            [game.game-map :as gmap]
            [game.constants :as consts]
            (game.client [input :as ci]
                         [hud :as hud])
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [input :as cmi]
                         [graphics :as gfx]
                         [items :as items])
            [game.math :as gmath]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math])
  (:use game.utils))

(defmulti process-event (fn [game-state event] (:type event)))

(defmethod process-event :default [game-state event])

(defmethod process-event :s-own-id [game-state {id :id}]
  {:new-game-state (assoc game-state :own-id id)})

(defn process-received-game-state [game-state]
  (let [curr-time (current-time-ms)]
    (assoc game-state :last-move curr-time)))

(defmethod process-event :s-game-state [game-state {new-game-state :game-state}]
  {:new-game-state (merge game-state
                          (process-received-game-state new-game-state))})

(defmethod process-event :s-move [game-state {:keys [positions]}]
  (let [positions (dissoc positions (:own-id game-state))]
    {:new-game-state
     (reduce (fn [gs [id pos]] (assoc-in gs [:chars id :new-pos] pos))
             game-state
             positions)}))

(defmethod process-event :s-char-death [game-state {:keys [id]}]
  {:new-game-state
   (if (= (:own-id game-state) id)
     (update-in game-state [:chars id] dissoc :target :attacking)
     (dissoc-in game-state [:chars id]))})

(defmethod process-event :s-spawn-corpse [game-state {:keys [id corpse]}]
  {:new-game-state (assoc-in game-state [:corpses id] corpse)})

(defmethod process-event :s-spawn-player [game-state {:keys [id player]}]
  {:new-game-state (update-in game-state [:chars id] merge player)})

(defmethod process-event :s-attack [game-state {:keys [target damage hit]}]
  (when hit
    {:new-game-state (update-in game-state [:chars target :hp] - damage)}))

(defmethod process-event :s-spawn-mobs [game-state {:keys [mobs]}]
  {:new-game-state (update-in game-state [:chars] merge mobs)})

(defmethod process-event :s-decay-corpses [game-state {:keys [ids]}]
  {:new-game-state (-> game-state
                     (update-in [:corpses] #(apply dissoc % ids))
                     (update-in [:looting] #(apply disj % ids)))})

(defmethod process-event :s-loot [game-state {:keys [corpse-id drops]}]
  {:new-game-state
   (-> game-state
       (assoc-in [:corpses corpse-id :drops] drops)
       (update-in [:looting] conj corpse-id))})

(defmethod process-event :s-loot-item-ok [game-state {:keys [from-path]}]
  {:new-game-state (ccfns/loot-item game-state from-path [:inv])})

(defmethod process-event :s-item-looted [game-state {:keys [from-path by left]}]
  {:new-game-state
   (if left
     (assoc-in game-state (conj from-path :quantity) left)
     (dissoc-in game-state from-path))})

(defn update-own-stats [{:keys [chars own-id gear] :as game-state}]
  (assoc game-state :chars
         (-> chars
             (assoc-in [own-id :gear] gear)
             (update-in [own-id] ccfns/update-stats)
             (update-in [own-id] merge (ccfns/sum-stats gear))
             (dissoc-in [own-id :gear]))))

(defmethod process-event :s-hp-update
  [{:keys [chars] :as game-state} {:keys [id-hp-vecs]}]
  {:new-game-state
   (assoc-in game-state [:chars]
             (reduce (fn [cs [id hp]] (assoc-in cs [id :hp] hp))
                     chars id-hp-vecs))})

(defmethod process-event :s-char-update [game-state {:keys [id updated]}]
  (let [new-level (:level updated)
        old-level (get-in game-state [:chars id :level])
        leveled-up (> new-level old-level)]
    {:new-game-state
     (cond-> (update-in game-state [:chars id] merge updated)
       (== (:own-id game-state) id) update-own-stats)
     :event (when leveled-up {:type :level-up :id id})}))

(defmethod process-event :changed-gear [game-state event]
  {:new-game-state (update-own-stats game-state)})

(defmethod process-event :level-up [game-state {:keys [id]}]
  (println "DING!"))

(defn calculate-base-movement-direction [key-state]
  (letfn [(adder [dx dy] (fn [[x y]] [(+ dx x) (+ dy y)]))]
    (cond-> [0 0]
      (:forward key-state) ((adder 0 1))
      (:back key-state) ((adder 0 -1))
      (:left key-state) ((adder -1 0))
      (:right key-state) ((adder 1 0))
      true (gmath/normalize))))

(defn update-looking-direction [{:keys [last-dir-update] :as game-state}]
  (when (> (current-time-ms) (+ last-dir-update consts/dir-update-interval))
    {:new-game-state
     (-> game-state
         (assoc :looking-dir (gfx/get-camera-dir))
         (assoc :last-dir-update (current-time-ms)))}))

(defn calculate-movement-direction
  [{:keys [base-move-dir own-id looking-dir] :as game-state}]
  (let [angle (gmath/angle-between-vecs [0 1] looking-dir)
        new-move-dir (map float (gmath/rotate-vec base-move-dir angle))
        old-move-dir (map float (get-in game-state [:chars own-id :move-dir]))]
    (when-not (rec== new-move-dir old-move-dir)
      {:new-game-state (assoc-in game-state [:chars own-id :move-dir]
                                 new-move-dir)
       :event {:type :new-dir}})))

(defmethod process-event :new-key-state [game-state {:keys [key-state]}]
  (let [id (:own-id game-state)
        new-base-dir (map float (calculate-base-movement-direction key-state))
        modifiers (select-keys key-state [:shift :alt :ctrl])
        new-game-state (assoc game-state
                              :base-move-dir new-base-dir
                              :modifiers modifiers)]
    {:new-game-state new-game-state}))

(defmethod process-event :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:chars own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    {:event {:type :c-move :pos pos :move-dir dir}}))

(defmethod process-event :toggle-attack [{id :own-id :as game-state} _]
  {:new-game-state (update-in game-state [:chars id :attacking] not)
   :event {:type :c-toggle-attack}})

(defmethod process-event :target [game-state _]
  (let [id (:own-id game-state)
        target (gfx/pick-target :chars)]
    (when target
      {:new-game-state (assoc-in game-state [:chars id :target] target)
       :event {:type :c-target :target target}})))

(defmethod process-event :loot [game-state _]
  (when-let [corpse (gfx/pick-target :corpses)]
    (when (ccfns/id-close-enough? game-state (:own-id game-state)
                                  corpse consts/loot-distance)
      {:event {:type :c-loot-corpse :corpse-id corpse}})))

(defmethod process-event :open-inv [game-state _]
  {:new-game-state (update-in game-state [:inv-open?] not)})

(defn my-inv? [path]
  (= :inv (first path)))

(defn my-gear? [path]
  (= :gear (first path)))

(defn my-stuff? [path]
  (or (my-inv? path) (my-gear? path)))

(defn drop-stack-onto-stack [game-state path]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state]
    {:new-game-state (dissoc game-state :on-mouse :on-mouse-quantity)
     :event {:type :c-move-quantity :from-path on-mouse :to-path path
             :quantity on-mouse-quantity}}))

(defn swap-places [game-state path]
    {:new-game-state (dissoc game-state :on-mouse)
     :event {:type :inv-swap :paths [(:on-mouse game-state) path]}})

(defn pick-up-quantity [game-state path quantity]
  {:new-game-state
   (-> game-state (assoc :on-mouse path) (assoc :on-mouse-quantity quantity))})

(defn pick-up-stack [game-state path]
  ;;; on-mouse-quantity should be positive
  (let [{:keys [ctrl]} (:modifiers game-state)
        quantity (:quantity (get-in game-state path))]
    {:new-game-state
     (-> game-state (assoc :on-mouse path)
         (assoc :on-mouse-quantity (if ctrl 1 quantity)))}))

(defn pick-up-item [game-state path]
  {:new-game-state (assoc game-state :on-mouse path)})

(defn pick-up-or-drop-item [game-state path]
  (let [{:keys [quantity id] :as clicked} (get-in game-state path)
        {:keys [on-mouse on-mouse-quantity]} game-state
        mouse-item (get-in game-state on-mouse)
        mouse-id (:id mouse-item)]
    (if on-mouse
      (if (and on-mouse-quantity
               (or (nil? clicked) (= id mouse-id)))
        (drop-stack-onto-stack game-state path)
        (swap-places game-state path))
      (when clicked
        (if quantity
          (pick-up-stack game-state path)
          (pick-up-item game-state path))))))

(defn loot-item [game-state path]
  {:event {:type :c-loot-item :from-path path}})

(defn get-prioritized-slot [{:keys [gear] :as game-state} item]
  (let [slots (:slots (items/all-info item))
        empty-slots (remove #(% gear) slots)]
    (first (concat empty-slots slots))))

(defn equip-item [game-state path]
  (let [item (get-in game-state path)
        slot (get-prioritized-slot game-state item)]
    (when slot
      {:event {:type :inv-swap :paths [[:gear slot] path]}})))

(defn activate-item [game-state path]
  (condp #(= %1 (first %2)) path
    :corpses (loot-item game-state path)
    :inv (equip-item game-state path)
    nil))

(defmethod process-event :hud-click [game-state {:keys [path button pressed]}]
  (cond
    (and pressed (= button 0)) (pick-up-or-drop-item game-state path)
    (and pressed (= button 1)) (activate-item game-state path)))

(defmethod process-event :inv-swap
  [game-state {[from to :as paths] :paths :as event}]
  (when (and (every? my-stuff? paths)
             (ccfns/possible-slot? game-state from to)
             (ccfns/possible-slot? game-state to from))
    (cond-> {:new-game-state (swap-in game-state from to)
             :events [{:type :c-rearrange-inv :paths paths}]}
      (some #{:gear} (map first paths))
      (update-in [:events] conj {:type :changed-gear}))))

(defmethod process-event :c-move-quantity
  [game-state {:keys [from-path to-path quantity] :as event}]
  {:new-game-state (ccfns/move-quantity game-state from-path to-path quantity)})

(defn move-out-own-inv [{:keys [own-id] :as game-state}]
  (-> game-state
      (move-in [:chars own-id :inv] [:inv])
      (move-in [:chars own-id :gear] [:gear])))

(defn login-and-recv-state [game-state net-sys name password stop?]
  (cc/update net-sys [{:type :c-login :username name :password password}])
  (loop [events (cc/get-events net-sys)]
    (when-not @stop?
      (if (== 2 (count (filter (fn [e] (contains? #{:s-game-state :s-own-id}
                                                  (:type e)))
                               events)))
        (move-out-own-inv
          (:new-game-state
            (ccfns/process-events process-event game-state events)))
        (recur (concat events (cc/get-events net-sys)))))))

(defn legal-pos? [{:keys [terrain] :as game-state} pos]
  (let [r consts/player-radius
        -r (- r)
        adder (fn [v] (map + pos v))]
    (every? gmap/walkable-type?
            (map #(get-in terrain (mapv (comp int math/floor) %))
                 (map adder [[r r] [r -r] [-r r] [-r -r]])))))

(defn update-looting [game-state new-pos]
  (let [get-pos #(get-in game-state [:corpses % :pos])]
    (set (filter #(ccfns/pos-close-enough? (get-pos %) new-pos
                                           consts/loot-distance)
                 (:looting game-state)))))

(defn move-self [{:keys [looting own-id chars move-time-delta] :as game-state}]
  (let [{:keys [pos move-dir speed] :as self} (chars own-id)
        new-pos (gmath/extrapolate-pos pos move-dir move-time-delta speed)]
    (if (legal-pos? game-state new-pos)
      (let [new-looting (update-looting game-state new-pos)
            ngs (-> game-state
                    (assoc-in [:chars own-id :pos] new-pos)
                    (assoc :looting new-looting))
            quitted (set/difference looting new-looting)]
        {:new-game-state ngs
         :event (when (seq quitted) {:type :c-quit-looting :ids quitted})})
      {:new-game-state (assoc-in game-state [:chars own-id :move-dir] [0 0])
       :event {:type :new-dir}})))

(defn move-toward-new-pos [{:keys [pos new-pos speed] :as char} time-delta _]
  (if new-pos
    (let [new-char (ccfns/move-toward-pos char time-delta new-pos)]
      (if (= (:pos new-char) new-pos)
        (dissoc new-char :new-pos)
        new-char))
    char))

(defn move-chars [game-state]
  (let [{:keys [chars move-time-delta last-move]} game-state]
    {:new-game-state
     (assoc-in game-state [:chars]
               (fmap #(move-toward-new-pos % move-time-delta last-move)
                     chars))}))

(defn initial-game-state []
  (-> (gmap/load-game-map) (dissoc :spawns) (assoc :base-move-dir [0 0])
      (assoc :last-dir-update 0) (assoc :looting #{})))

(defn get-subsystem-events [_ systems]
  {:events (mapcat cc/get-events systems)})

(defn make-process-and-send-fn [networking-system]
  (fn [game-state events]
    (let [{:keys [new-game-state new-events]}
          (ccfns/process-events process-event game-state events)]
      (cc/update networking-system (concat events new-events))
      {:new-game-state new-game-state :events new-events})))

(defrecord Client [app]
  cc/Lifecycle
  (start [this]
    (cc/start app)
    this)
  (stop [this]
    (cc/stop app)
    this))

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
            (reset! graphics-system
                    (gfx/init-graphics-system app (:terrain @game-state-atom)))
            (reset! hud-system
                    (hud/init-hud-system app))
            (reset! input-system
                    (cmi/init-input-system
                      input-manager (ci/load-key-bindings)))
            (dorun (map cc/start (get-subsystems)))
            (reset! game-state-atom
                    (login-and-recv-state @game-state-atom @networking-system
                                          "leif" "star" stop?))
            (gfx/set-up-camera @graphics-system @game-state-atom)))
        simple-update-fn
        (fn []
          (Thread/sleep 1)
          (let [hook (make-process-and-send-fn @networking-system)
                {:keys [new-game-state events]}
                (ccfns/call-update-fns @game-state-atom [] hook
                  (get-subsystem-events (get-subsystems))
                  (ccfns/calculate-move-time-delta)
                  (update-looking-direction)
                  (calculate-movement-direction)
                  (move-self)
                  (move-chars))]
            (cc/update @graphics-system new-game-state)
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
          (dorun (map cc/stop (get-subsystems)))
          (.stop this))]
    [stop? (ccfns/create-jme3-app start-fn stop-fn
                                  simple-init-fn simple-update-fn
                                  init-app-settings-fn)]))

(defn create-non-jme3-app [networking-system game-state-atom]
  (let [stop? (atom false)]
    (reify cc/Lifecycle
      (start [this]
        (error-printing-future
          (reset! game-state-atom
                  (login-and-recv-state
                    @game-state-atom networking-system "leif" "star"
                    stop?))
          ((fn []
             (reset! game-state-atom
                     (:new-game-state
                       (ccfns/process-events process-event @game-state-atom
                                             networking-system)))
             (if @stop? nil (recur))))))
      (stop [this]
        (reset! stop? true)))))

(defn init-client [address port headless]
  (let [game-state-atom (atom (initial-game-state))
        networking-system (atom (net/init-client-net-sys address port))
        [stop? app]
        (if headless
          (create-non-jme3-app networking-system game-state-atom)
          (create-client-jme3-app networking-system game-state-atom))]
    [stop? (->Client app)]))
