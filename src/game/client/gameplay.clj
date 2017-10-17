(ns game.client.gameplay
  (:require
   [clojure.set :as set]
   [game.client.base :as b]
   [game.client.graphics :as gfx]
   [game.client.inventory :as inv]
   [game.common.core-functions :as ccfns]
   [game.common.spells :as csp]
   [game.constants :as consts]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.utils :refer :all]))

(defmethod b/process-event :s-own-id [game-state {id :id}]
  (assoc game-state :own-id id))

(defn process-received-game-state [game-state]
  (let [curr-time (current-time-ms)]
    (assoc game-state :last-move curr-time)))

(defmethod b/process-event :s-game-state [game-state {new-gs :game-state}]
  (merge game-state (process-received-game-state new-gs)))

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

(defn initial-game-state []
  (-> {} (assoc :base-move-dir [0 0]) (assoc :last-dir-update 0)
      (assoc :looting #{})))

(defmethod b/process-event :s-move-chars [game-state {:keys [positions]}]
  (reduce (fn [gs [id pos]] (assoc-in gs [:chars id :new-pos] pos))
          game-state
          (dissoc positions (:own-id game-state))))

(defmethod b/process-event :new-dir [game-state event]
  (let [own-id (:own-id game-state)
        self (get-in game-state [:chars own-id])
        pos (map float (:pos self))
        dir (map float (:move-dir self))]
    (b/enqueue-events {:type :c-move :pos pos :move-dir dir})))

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

(defmethod b/process-event :s-char-death [game-state {:keys [id]}]
  (if (= (:own-id game-state) id)
    (update-in game-state [:chars id] dissoc :target :attacking)
    (dissoc-in game-state [:chars id])))

(defmethod b/process-event :s-loot [game-state {:keys [corpse-id drops]}]
  (-> game-state
    (assoc-in [:corpses corpse-id :drops] drops)
    (update :looting conj corpse-id)))

(defmethod b/process-event :s-spawn-corpse [game-state {:keys [id corpse]}]
  (assoc-in game-state [:corpses id] corpse))

(defmethod b/process-event :s-spawn-player [game-state {:keys [id player]}]
  (update-in game-state [:chars id] merge player))

(defmethod b/process-event :s-attack
  [game-state {:keys [target damage hit] :as event}]
  (when hit
    (update-in game-state [:chars target :hp] - damage)))

(defmethod b/process-event :s-regen-tick [game-state {:keys [update-map]}]
  (update game-state :chars (partial merge-with merge) update-map))

(defmethod b/process-event :s-heal [game-state {:keys [target by amount]}]
  ;; The server makes sure the amount does not make the :hp go over :max-hp
  (update-in game-state [:chars target :hp] + amount))

(defmethod b/process-event :s-spawn-mobs [game-state {:keys [mobs]}]
  (update game-state :chars merge mobs))

(defmethod b/process-event :s-decay-corpses [game-state {:keys [ids]}]
  (-> game-state
    (update :corpses #(apply dissoc % ids))
    (update :looting #(apply disj % ids))))

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

(defmethod b/process-event :s-spell-cast
  [game-state {:keys [by spell mana-cost]}]
  (update-in game-state [:chars by :mana] - mana-cost))
