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
                         [items :as items]
                         [spells :as csp])
            [game.math :as gmath]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math])
  (:use game.utils))

(def event-queue (ref []))

(def all-events-queue (ref []))

(def enqueue-events (ccfns/enqueue-fn event-queue all-events-queue))

(defmulti process-event (fn [game-state event] (:type event)))

(defmethod process-event :default [game-state event])

(defmethod process-event :s-own-id [game-state {id :id}]
  (assoc game-state :own-id id))

(defn process-received-game-state [game-state]
  (let [curr-time (current-time-ms)]
    (assoc game-state :last-move curr-time)))

(defmethod process-event :s-game-state [game-state {new-game-state :game-state}]
  (merge game-state (process-received-game-state new-game-state)))

(defmethod process-event :s-move [game-state {:keys [positions]}]
  (let [positions (dissoc positions (:own-id game-state))]
    (reduce (fn [gs [id pos]] (assoc-in gs [:chars id :new-pos] pos))
            game-state
            positions)))

(defmethod process-event :s-char-death [game-state {:keys [id]}]
  (if (= (:own-id game-state) id)
    (update-in game-state [:chars id] dissoc :target :attacking)
    (dissoc-in game-state [:chars id])))

(defmethod process-event :s-spawn-corpse [game-state {:keys [id corpse]}]
  (assoc-in game-state [:corpses id] corpse))

(defmethod process-event :s-spawn-player [game-state {:keys [id player]}]
  (update-in game-state [:chars id] merge player))

(defmethod process-event :s-attack
  [game-state {:keys [target damage hit] :as event}]
  (when hit
    (update-in game-state [:chars target :hp] - damage)))

(defmethod process-event :s-spawn-mobs [game-state {:keys [mobs]}]
  (update game-state :chars merge mobs))

(defmethod process-event :s-decay-corpses [game-state {:keys [ids]}]
  (-> game-state
      (update :corpses #(apply dissoc % ids))
      (update :looting #(apply disj % ids))))

(defmethod process-event :s-loot [game-state {:keys [corpse-id drops]}]
  (-> game-state
      (assoc-in [:corpses corpse-id :drops] drops)
      (update :looting conj corpse-id)))

(defmethod process-event :s-loot-item-ok [game-state {:keys [from-path]}]
  (ccfns/loot-item game-state from-path [:inv]))

(defmethod process-event :s-item-looted [game-state {:keys [from-path by left]}]
  (if left
    (assoc-in game-state (conj from-path :quantity) left)
    (dissoc-in game-state from-path)))

(defmethod process-event :s-regen-tick [game-state {:keys [update-map]}]
  (update game-state :chars (partial merge-with merge) update-map))

(defmethod process-event :s-heal [game-state {:keys [target by amount]}]
  ;; The server makes sure the amount does not make the :hp go over :max-hp
  (update-in game-state [:chars target :hp] + amount))

(defn update-own-stats [{:keys [own-id gear] :as game-state}]
  (update-in game-state [:chars own-id]
             #(-> %
                  (assoc :gear gear)
                  ccfns/update-stats
                  (merge (ccfns/sum-stats gear))
                  (dissoc :gear))))

(defmethod process-event :s-char-update [game-state {:keys [id updated]}]
  (let [new-level (:level updated)
        old-level (get-in game-state [:chars id :level])
        leveled-up (> new-level old-level)]
    (when leveled-up
      (enqueue-events {:type :level-up :id id}))
    (cond-> (update-in game-state [:chars id] merge updated)
      (== (:own-id game-state) id) update-own-stats)))

(defmethod process-event :changed-gear [game-state event]
  (update-own-stats game-state))

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

(defmethod process-event :new-key-state [game-state {:keys [key-state]}]
  (let [new-base-dir (map float (calculate-base-movement-direction key-state))
        modifiers (select-keys key-state [:shift :alt :ctrl])
        new-game-state (assoc game-state
                              :base-move-dir new-base-dir
                              :modifiers modifiers)]
    new-game-state))

(defmethod process-event :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:chars own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    (enqueue-events {:type :c-move :pos pos :move-dir dir})))

(defmethod process-event :toggle-attack [{id :own-id :as game-state} _]
  (enqueue-events {:type :c-toggle-attack})
  (update-in game-state [:chars id :attacking] not))

(defn init-destroy-item [game-state]
  (assoc game-state :destroying-item true))

(defn new-target [game-state]
  (let [id (:own-id game-state)
        target (gfx/pick-target :chars)]
    (when target
      (enqueue-events {:type :c-target :target target})
      (assoc-in game-state [:chars id :target] target))))

(defmethod process-event :left-click [game-state _]
  (if (:on-mouse game-state)
    (init-destroy-item game-state)
    (new-target game-state)))

(defn destroy-item [game-state]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state]
    (enqueue-events {:type :c-destroy-item :path on-mouse
                     :quantity on-mouse-quantity})
    (-> (ccfns/destroy-item game-state on-mouse on-mouse-quantity)
        (dissoc :on-mouse :on-mouse-quantity))))

(defmethod process-event :destroy-item [game-state {:keys [destroy]}]
  (dissoc
    (if destroy
      (destroy-item game-state)
      game-state)
    :destroying-item))

(defmethod process-event :right-click [game-state _]
  (when-let [corpse (gfx/pick-target :corpses)]
    (when (ccfns/id-close-enough? game-state (:own-id game-state)
                                  corpse consts/loot-distance)
      (enqueue-events {:type :c-loot-corpse :corpse-id corpse}))))

(defmethod process-event :spell
  [{:keys [chars own-id] :as game-state} {:keys [number]}]
  (when-lets [target-id (get-in chars [own-id :target])
              _ (chars target-id)
              {:keys [spell last-cast]} (get-in game-state [:spells number])
              cd (get-in csp/spells [spell :cooldown])
              curr-time (current-time-ms)
              _ (> curr-time (+ last-cast (* 1000 cd)))]
    (enqueue-events {:type :c-cast-spell :number number
                     :target target-id})
    (assoc-in game-state [:spells number :last-cast] (+ curr-time 1e6))))

(defmethod process-event :s-spell-response [game-state event]
  (let [{:keys [response number]} event]
    (assoc-in game-state [:spells number :last-cast]
              (case response
                :out-of-range (do (println "Out of range!") 0)
                :cooldown (do (println "Cooldown!") 0)
                :out-of-mana (do (println "Out of mana!") 0)
                :ok (current-time-ms)))))

(defmethod process-event :s-spell-cast [game-state {:keys [by spell mana-cost]}]
  (update-in game-state [:chars by :mana] - mana-cost))

(defmethod process-event :open-inv [game-state _]
  (update game-state :inv-open? not))

(defn my-inv? [path]
  (= :inv (first path)))

(defn my-gear? [path]
  (= :gear (first path)))

(defn my-stuff? [path]
  (or (my-inv? path) (my-gear? path)))

(defn drop-stack-onto-stack [game-state path]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state]
    (enqueue-events {:type :move-quantity :from-path on-mouse :to-path path
                     :quantity on-mouse-quantity})))

(defn swap-places [game-state path]
  (enqueue-events {:type :inv-swap :paths [(:on-mouse game-state) path]}))

(defn pick-up-quantity [game-state path quantity]
  (-> game-state (assoc :on-mouse path) (assoc :on-mouse-quantity quantity)))

(defn pick-up-stack [game-state path]
  (let [{:keys [ctrl]} (:modifiers game-state)
        quantity (:quantity (get-in game-state path))]
    (-> game-state (assoc :on-mouse path)
        (assoc :on-mouse-quantity (if ctrl 1 quantity)))))

(defn pick-up-item [game-state path]
  (assoc game-state :on-mouse path))

(defn pick-up-or-drop-item [game-state path]
  (let [{:keys [quantity id] :as clicked} (get-in game-state path)
        {:keys [on-mouse on-mouse-quantity]} game-state
        mouse-item (get-in game-state on-mouse)
        mouse-id (:id mouse-item)]
    (if on-mouse
      (do
        (if (and on-mouse-quantity
                 (or (nil? clicked) (= id mouse-id)))
          (drop-stack-onto-stack game-state path)
          (swap-places game-state path))
        (dissoc game-state :on-mouse :on-mouse-quantity))
      (when clicked
        (if quantity
          (pick-up-stack game-state path)
          (pick-up-item game-state path))))))

(defn loot-item [game-state path]
  (enqueue-events {:type :c-loot-item :from-path path}))

(defn get-prioritized-slot [{:keys [gear] :as game-state} item]
  (let [slots (:slots (items/all-info item))
        empty-slots (remove #(% gear) slots)]
    (first (concat empty-slots slots))))

(defn equip-item [game-state path]
  (when-lets [item (get-in game-state path)
              slot (get-prioritized-slot game-state item)]
    (enqueue-events {:type :inv-swap :paths [[:gear slot] path]})))

(defn activate-item [game-state path]
  (condp #(= %1 (first %2)) path
    :corpses (loot-item game-state path)
    :inv (equip-item game-state path)
    nil))

(defmethod process-event :inv-click [game-state {:keys [path button pressed]}]
  (-> (cond
        (and pressed (= button consts/mouse-left) (#{:inv :gear} (path 0)))
        (pick-up-or-drop-item game-state path)
        (and pressed (= button consts/mouse-right))
        (activate-item game-state path))
      (#(if %
          (dissoc % :destroying-item)
          (dissoc game-state :destroying-item)))))

(defmethod process-event :inv-swap [game-state {paths :paths :as event}]
  (when (every? my-stuff? paths)
    (enqueue-events {:type :c-rearrange-inv :paths paths})
    (ccfns/inv-swap game-state paths enqueue-events nil)))

(defmethod process-event :move-quantity
  [game-state {:keys [from-path to-path quantity] :as event}]
  (when (every? my-stuff? [from-path to-path])
    (enqueue-events (assoc event :type :c-move-quantity))
    (ccfns/move-quantity game-state from-path to-path quantity)))

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
        (when (seq quitted)
          (enqueue-events {:type :c-quit-looting :ids quitted}))
        ngs)
      (do
        (enqueue-events {:type :new-dir})
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
  (let [angle (gmath/angle-between [0 1] looking-dir)
        new-move-dir (map float (gmath/rotate-vec base-move-dir angle))
        old-move-dir (map float (get-in game-state [:chars own-id :move-dir]))]
    (when-not (rec== new-move-dir old-move-dir)
      (enqueue-events {:type :new-dir})
      (assoc-in game-state [:chars own-id :move-dir] new-move-dir))))

(defn initial-game-state []
  (-> {} (assoc :base-move-dir [0 0]) (assoc :last-dir-update 0)
      (assoc :looting #{})))

(defn get-subsystem-events [_ systems]
  (apply enqueue-events (mapcat cc/get-events systems)))

(defn process-events [game-state events]
  (reduce (fn [gs e] (or (process-event gs e) gs))
          game-state events))

(defn make-process-and-send-fn [networking-system]
  (ccfns/make-process-and-send-fn
    (fn [game-state events]
      (cc/update networking-system events)
      (process-events game-state events))
    nil event-queue))

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
                    (cmi/init-input-system
                      input-manager (ci/load-key-bindings)))
            (dorun (map cc/start (remove #{@networking-system}
                                         (get-subsystems))))
            (gfx/set-up-camera @graphics-system @game-state-atom)))
        simple-update-fn
        (fn []
          (Thread/sleep 1)
          (let [hook (make-process-and-send-fn @networking-system)
                new-game-state
                (ccfns/call-update-fns* @game-state-atom hook
                  (get-subsystem-events (get-subsystems))
                  (ccfns/calculate-move-time-delta)
                  (update-looking-direction)
                  (calculate-movement-direction)
                  (move-self)
                  (move-chars))
                events (ccfns/reset-queue all-events-queue)]
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
                     (ccfns/process-events process-event @game-state-atom
                                           networking-system))
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
