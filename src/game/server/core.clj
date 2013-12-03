(ns game.server.core
  (:require [clojure.data.priority-map :as pm]
            [game.networking.core :as net]
            (game.common [core :as cc]
                         [core-functions :as ccfns])
            (game.key-value-store [core :as kvs.core]
                                  [protocols :as kvs])
            (game [math :as math]
                  [game-map :as gmap]
                  [mobs :as mobs]
                  [constants :as consts]))
  (:use game.utils))

(defn new-player [username]
  {:name username
   :speed 1
   :pos [0 0]
   :old-recv-pos [0 0]
   :move-dir [0 0]
   :type :player
   :attacking false
   :hp 100
   :max-hp 100
   :dmg 15
   :delay 2
   :last-attack 0})

(let [game-id-counter (atom 0)
      net->game (atom {})
      game->net (atom {})]
  (defn new-game-id
    ([] (swap! game-id-counter inc))
    ([net-id]
     (let [game-id (swap! game-id-counter inc)]
       (swap! net->game assoc net-id game-id)
       (swap! game->net assoc game-id net-id)
       game-id)))
  (defn net-id->game-id [net-id]
    (@net->game net-id))
  (defn game-id->net-id [game-id]
    (@game->net game-id)))

(defmulti process-msg-purely (fn [game-state msg] (:type msg)))

(defmethod process-msg-purely :c-move [game-state {:keys [id pos move-dir]}]
  {:new-game-state
   (-> game-state
       (update-in [:chars id] merge
                  {:recv-pos pos :move-dir move-dir
                   :recv-time (current-time-ms)})
       (update-in [:chars id :recv-this-frame] conj pos))})

(defmethod process-msg-purely :c-toggle-attack [game-state {:keys [id]}]
  {:new-game-state (update-in game-state [:chars id :attacking] not)})

(defmethod process-msg-purely :c-target [game-state {:keys [id target]}]
  {:new-game-state (assoc-in game-state [:chars id :target] target)})

(defmethod process-msg-purely :default [game-state _]
  {:new-game-state game-state})

(defmulti process-msg (fn [game-state msg key-value-store] (:type msg)))

(defmethod process-msg :c-login [game-state msg key-value-store]
  (let [{:keys [id username password]} msg
        curr-time (current-time-ms)
        player (assoc (or (kvs/load key-value-store username)
                          (new-player username))
                      :old-recv-time curr-time :last-move curr-time)]
    {:new-game-state (-> game-state
                         (assoc-in [:chars id] player)
                         (update-in [:player-ids] conj id))
     :event {:type :login :id id}}))

(defmethod process-msg :connect [game-state _ _]
  {:new-game-state game-state})

(defmethod process-msg :disconnect [game-state _ _]
  {:new-game-state game-state})

(defmethod process-msg :default [game-state msg _]
  (process-msg-purely game-state msg))

(defmulti process-event (fn [game-state event] (:type event)))

(defn spawn-mob [spawn-id spawns curr-time]
  (let [mob-type (-> spawn-id spawns :type mobs/mobs)]
    (assoc mob-type
           :spawn spawn-id
           :pos (-> spawn-id spawns :pos)
           :move-dir [0 0]
           :last-move curr-time
           :last-attack 0
           :max-hp (:hp mob-type)
           :dmg (/ (:dmg mob-type) 10))))

(defmethod process-event :spawn-mobs
  [{:keys [to-spawn spawns] :as game-state} {ids :mob-ids}]
  (let [curr-time (current-time-ms)
        new-mobs (map (partial* spawn-mob curr-time spawns)
                      ids)
        new-mobs-map (zipmap (repeatedly new-game-id) new-mobs)
        num-mobs (count new-mobs-map)]
    {:new-game-state
     (-> game-state
         (update-in [:chars] merge new-mobs-map)
         (assoc :to-spawn (call-times num-mobs pop to-spawn)))}))

(defmethod process-event :attack [game-state event]
  {:new-game-state game-state})

(defmethod process-event :default [game-state event]
  {:new-game-state game-state})

(defn prepare-chars-for-sending [chars]
  (fmap (fn [char] (-> char (select-keys [:speed :name :pos :type])
                       (assoc :pos (map float (:pos char)))))
        chars))

(defn prepare-for-sending [game-state]
  (-> game-state
      (update-in [:chars] prepare-chars-for-sending)
      (select-keys [:chars])))

(defmulti produce-client-msgs (fn [game-state event] (:type event)))

(defmethod produce-client-msgs :login [game-state {id :id}]
  (let [all-players (:player-ids game-state)
        game-state-to-send (prepare-for-sending game-state)]
    [[[id] {:type :s-game-state :game-state game-state-to-send}]
     [all-players
      {:type :s-login :id id :player (get-in game-state-to-send [:chars id])}]
     [[id] {:type :s-own-id :id id}]]))

(defmethod produce-client-msgs :chars-moved [game-state {ids :moved-ids}]
  (let [all-players (:player-ids game-state)]
    (for [id ids]
      [(disj all-players id)
       {:type :s-move :id id
        :pos (map float (get-in game-state [:chars id :pos]))}])))

(defmethod produce-client-msgs :default [_ _]
  nil)

(defn move-from-recv-pos
  [{:keys [recv-time recv-pos] :as player}]
  ;; TODO: Use old-recv-pos, recv-this-frame, recv-time and old-recv-time to
  ;; calculate the distance and time the player has moved this frame, and save
  ;; that somewhere so that the server can check that the player is not moving
  ;; too fast.
  (-> (ccfns/extrapolate-char player recv-pos recv-time)
      (assoc :old-recv-pos recv-pos :old-recv-time recv-time)
      (dissoc :recv-this-frame)))

(defn actually-move-char
  [{:keys [pos last-move move-dir recv-this-frame speed] :as char}]
  (if recv-this-frame
    (move-from-recv-pos char)
    (ccfns/extrapolate-char char pos last-move)))

(defn move-char [{pos :pos :as char}]
  (let [new-char (actually-move-char char)
        new-pos (:pos new-char)]
    (assoc new-char :moved-this-frame (not (rec== pos new-pos)))))

(defn spawn-mobs [{:keys [to-spawn] :as game-state}]
  (let [curr-time (current-time-ms)
        time-to-spawn (fn [[id spawn-time]] (> curr-time spawn-time))
        ids (keys (take-while time-to-spawn to-spawn))]
    (when (seq ids)
      {:event {:type :spawn-mobs
               :mob-ids ids}})))

(defn check-if-moved [game-state]
  (when-let [moved (reduce (fn [moved [id char]]
                             (if (:moved-this-frame char)
                               (conj moved id)
                               moved))
                           nil
                           (:chars game-state))]
    {:event {:type :chars-moved :moved-ids moved}}))

(defn cooled-down? [{:keys [last-attack delay]}]
  (> (current-time-ms) (+ last-attack (* 1000 delay))))

(defn close-enough? [game-state attacker-id target-id]
  (let [curr-time (current-time-ms)
        get-pos (fn [id] (get-in game-state [:chars id :pos]))
        attacker-pos (get-pos attacker-id)
        target-pos (get-pos target-id)]
    (> consts/attack-distance (math/distance attacker-pos target-pos))))

(defn calculate-new-last-attack [{:keys [last-attack delay]}]
  (let [curr-time (current-time-ms)]
    (if (> (+ last-attack (* 1000 (+ delay consts/attack-delay-leeway)))
           curr-time)
      (+ last-attack (* 1000 delay))
      curr-time)))

(defn let-chars-attack [game-state]
  (let [generate-attack-event
        (fn [[id {:keys [target attacking dmg last-attack delay] :as char}]]
          (when (and attacking target
                     (cooled-down? char)
                     (close-enough? game-state id target))
            {:id id :type :attack :target target :damage dmg
             :last-attack (calculate-new-last-attack char)}))]
    {:events (remove nil? (map generate-attack-event (:chars game-state)))}))

(defn process-network-msgs [game-state net-map key-value-store]
  (ccfns/process-network-msgs game-state net-map process-msg key-value-store))

(defn process-events [game-state input-events]
  (let [events-q (reduce conj clojure.lang.PersistentQueue/EMPTY input-events)]
    (loop [game-state game-state events-q events-q new-events []]
      (if (seq events-q)
        (let [{:keys [new-game-state event events]}
              (process-event game-state (first events-q))
              events (if event (conj events event) events)]
          (recur new-game-state (reduce conj (pop events-q) events)
                 (reduce conj new-events events)))
        {:new-game-state game-state :new-events new-events}))))

(defn main-update [game-state {:keys [send-msg] :as net-map} key-value-store]
  (Thread/sleep 50)
  (let [{:keys [new-game-state events]}
        (ccfns/call-update-fns game-state []
          (process-network-msgs net-map key-value-store)
          (ccfns/move-chars move-char)
          (check-if-moved)
          (spawn-mobs)
          (let-chars-attack))
        {:keys [new-events new-game-state]} (process-events new-game-state events)
        all-events (concat events new-events)
        to-client-msgs (mapcat (partial produce-client-msgs new-game-state)
                               all-events)]
    (doseq [[ids to-client-msg] to-client-msgs]
      (send-msg ids to-client-msg))
    new-game-state))

(defrecord Server [net-map key-value-store game-state stop?]
  cc/Lifecycle
  (start [this]
    (cc/start (:net-sys net-map))
    (cc/start key-value-store)
    (start-new-thread "server"
      ((fn [game-state]
         (if @stop?
           nil
           (recur (main-update game-state net-map key-value-store))))
       game-state))
    this)
  (stop [this]
    (reset! stop? true)
    (cc/stop key-value-store)
    (cc/stop (:net-sys net-map))
    this))

(defn create-to-spawn-queue [spawns]
  (apply pm/priority-map (interleave (keys spawns) (repeat 0))))

(defn create-game-state []
  (-> {:chars {} :player-ids #{}}
      (merge (gmap/load-game-map))
      (as-> gs
        (assoc gs :to-spawn (create-to-spawn-queue (:spawns gs))))))

(defn init-server [port]
  (let [game-state (create-game-state)
        stop? (atom false)
        {:keys [net-sys get-msg send-msg]} (net/construct-server-net-sys
                                             port
                                             cc/connect-msg
                                             cc/disconnect-msg)
        new-get-msg (fn []
                      (when-let [{:keys [id msg]} (get-msg)]
                        (let [game-msg (ccfns/msg->map msg)
                              game-id (if (= cc/connect-msg game-msg)
                                        (new-game-id id)
                                        (net-id->game-id id))]
                          (assoc game-msg :id game-id))))
        new-send-msg (fn [game-ids msg-map]
                       (let [net-msg (ccfns/map->msg msg-map)]
                         (doseq [game-id game-ids]
                           (send-msg (game-id->net-id game-id) net-msg))))
        net-map {:net-sys net-sys :get-msg new-get-msg :send-msg new-send-msg}
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server net-map key-value-store game-state stop?)))
