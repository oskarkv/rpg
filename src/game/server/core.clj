(ns game.server.core
  (:require [clojure.data.priority-map :as pm]
            [game.networking.core :as net]
            (game.common [core :as cc]
                         [core-functions :as ccfns])
            (game.key-value-store [core :as kvs.core]
                                  [protocols :as kvs])
            (game [math :as gmath]
                  [game-map :as gmap]
                  [mobs :as mobs]
                  [constants :as consts])
            (game.server [pathfinding :as pf])
            [clojure.math.numeric-tower :as math])
  (:use game.utils))

(defn new-player [username]
  {:name username
   :speed 2
   :pos [1 1]
   :bind-spot [1 1]
   :move-dir [0 0]
   :type :player
   :attacking false
   :hp 100
   :max-hp 100
   :dmg 35
   :delay 1
   :last-attack 0
   :level 1
   :exp 0})

(defn attack-nearest [game-state mob players]
  (let [{:keys [pos]} mob
        [nearest dist]
        (reduce (fn [[nearest-id dist :as old] [id char]]
                  (let [new-dist (gmath/distance (:pos char) pos)]
                    (if (< new-dist dist)
                      [id new-dist]
                      old)))
                [nil Integer/MAX_VALUE]
                players)]
    (if (< dist consts/attack-nearest-threshold)
      (assoc mob :target nearest :attacking true)
      (assoc mob :target nil))))

(defn decide-mob-path [game-state mob]
  (if (:target mob)
    (let [{:keys [target pos]} mob
          {:keys [terrain]} game-state
          target-pos (get-in game-state [:chars target :pos])
          path (pf/find-path terrain pos target-pos consts/player-radius)]
      (assoc mob :path path))
    mob))

(defn call-on-all-mobs [{:keys [chars] :as game-state} f]
  (let [mobs (filter (fn [[id char]] (= :mob (:type char))) chars)
        new-mobs (fmap #(f game-state %) mobs)]
    {:new-game-state (assoc game-state :chars (into chars new-mobs))}))

(defn decide-mob-actions [{:keys [chars player-ids] :as game-state}]
  (let [players (select-keys chars player-ids)
        ai-fn #(attack-nearest % %2 players)]
    (call-on-all-mobs game-state ai-fn)))

(defn decide-mob-paths [game-state]
  (call-on-all-mobs game-state decide-mob-path))

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
                  {:recv-pos pos :move-dir (gmath/normalize move-dir)
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
        player (or (kvs/load key-value-store username)
                   (new-player username))]
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

(defn spawn-mob [spawn-id spawns]
  (let [mob-type (-> spawn-id spawns :type mobs/mobs)]
    (assoc mob-type
           :type :mob
           :spawn spawn-id
           :pos (-> spawn-id spawns :pos)
           :move-dir [0 0]
           :last-attack 0
           :max-hp (:hp mob-type)
           :delay 1
           :level 1)))

(defmethod process-event :spawn-ids
  [{:keys [to-spawn spawns] :as game-state} {ids :ids}]
  (let [new-mobs (map #(spawn-mob % spawns) ids)
        new-mobs-map (zipmap (repeatedly new-game-id) new-mobs)
        num-mobs (count new-mobs-map)
        event {:type :spawn-mobs :mobs new-mobs-map}]
    {:new-game-state
     (-> game-state
         (update-in [:chars] merge new-mobs-map)
         (assoc :to-spawn (call-times num-mobs pop to-spawn)))
     :event event}))

(defn register-damage [char id damage]
  (let [tag #(if % % id)
        set-damage #(if % (+ % damage) damage)]
    (-> char
        (update-in [:tagged-by] tag)
        (update-in [:damaged-by id] set-damage))))

(defmethod process-event :attack [game-state event]
  (let [{:keys [id target damage last-attack]} event
        register-damage' #(if (= :mob (:type %))
                            (register-damage % id damage)
                            %)
        new-game-state (-> game-state
                           (update-in [:chars target :hp] - damage)
                           (assoc-in [:chars id :last-attack] last-attack)
                           (update-in [:chars target] register-damage'))
        death (when (<= (get-in new-game-state [:chars target :hp]) 0)
                {:type :death :id target :by id})]
    {:new-game-state new-game-state :event death}))

(defn exp-per-mob [level]
  (let [level (dec level)]
    (+ 10 (* 5 level))))

(defn mobs-per-level [level]
  (let [level (dec level)]
    (+ 10 (* 5 level) (math/round (math/expt level 1.5)))))

(defn exp-to-level [level]
  (let [level (dec level)]
    (* (exp-per-mob level) (mobs-per-level level))))

(defn exp-modifier [mob-level player-level]
  (let [diff (- mob-level player-level)]
    (if (< diff -10)
      0
      (+ 1 (* 0.1 diff)))))

(defn exp-gained [mob-level player-level]
  (* (exp-modifier mob-level player-level) (exp-per-mob mob-level)))

(defn give-exp [{:keys [level exp] :as char} new-exp]
  (let [exp-sum (+ exp new-exp)
        req-exp (exp-to-level (inc level))]
    (if (> exp-sum req-exp)
      (-> char
          (update-in [:level] inc)
          (assoc-in [:exp] (- exp-sum req-exp)))
      (update-in char [:exp] + new-exp))))

(defn distribute-exp [game-state mob]
  (let [{:keys [damaged-by tagged-by]} mob
        total-damage (apply + (vals damaged-by))
        tagged-damage (damaged-by tagged-by)
        all-exp (exp-gained (:level mob)
                            (get-in game-state [:chars tagged-by :level]))
        actual-exp (* all-exp (/ tagged-damage total-damage))]
    (update-in game-state [:chars tagged-by] give-exp actual-exp)))

(defn mob-death [game-state {:keys [id by]}]
  (let [mob (get-in game-state [:chars id])
        spawn-id (:spawn mob)
        respawn-time (get-in game-state [:spawns spawn-id :respawn-time])]
    {:new-game-state
     (-> game-state
         (dissoc-in [:chars id])
         (update-in [:to-spawn] conj
                    [spawn-id (+ (current-time-ms) (* 1000 respawn-time))])
         (distribute-exp mob))}))

(defn player-death [game-state {:keys [id by]}]
  (let [char-set (fn [gs key val] (assoc-in gs [:chars id key] val))
        char-get (fn [key] (get-in game-state [:chars id key]))]
    {:new-game-state
     (-> game-state
         (char-set :pos (char-get :bind-spot))
         (char-set :hp (char-get :max-hp))
         (char-set :target nil)
         (char-set :attacking false))}))

(defmethod process-event :death [game-state {:keys [id] :as event}]
  (if (= :mob (get-in game-state [:chars id :type]))
    (mob-death game-state event)
    (player-death game-state event)))

(defmethod process-event :default [game-state event]
  {:new-game-state game-state})

(defn prepare-char-for-sending [char]
  (-> char (select-keys [:speed :name :pos :type :hp :max-hp])
      (assoc :pos (map float (:pos char)))))

(defn prepare-chars-for-sending [chars]
  (fmap prepare-char-for-sending chars))

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
  [[(:player-ids game-state)
    {:type :s-move :positions
     (into {} (for [id ids
                    :let [pos (get-in game-state [:chars id :pos])]
                    :when pos]
                [id (map float pos)]))}]])

(defmethod produce-client-msgs :attack [game-state event]
  [[(:player-ids game-state) (merge {:type :s-attack}
                                   (select-keys event [:target :damage]))]])

(defmethod produce-client-msgs :spawn-mobs [game-state {mobs :mobs}]
  [[(:player-ids game-state)
    {:type :s-spawn-mobs :mobs (prepare-chars-for-sending mobs)}]])

(defmethod produce-client-msgs :death [game-state {:keys [id by]}]
  (let [all-players (:player-ids game-state)
        char (get-in game-state [:chars id])
        msgs-vec [[all-players {:type :s-char-death :id id}]]]
    (if (= :player (:type char))
      (conj msgs-vec
            [all-players {:type :s-spawn-player
                          :id-char [id (prepare-char-for-sending char)]}])
      msgs-vec)))

(defmethod produce-client-msgs :default [_ _]
  nil)

(defn move-player* [char time-delta last-move]
  (let [{:keys [pos move-dir recv-pos recv-time speed]} char]
    (if recv-pos
      (recur (-> char (dissoc :recv-pos) (assoc :pos recv-pos))
             (/ (- last-move recv-time) 1000.0)
             last-move)
      (assoc char :pos (gmath/extrapolate-pos pos move-dir time-delta speed)))))

(defn move-mob* [{:keys [pos speed path target] :as mob} time-delta chars]
  (let [target-pos (get-in chars [target :pos])]
    (if (and path (> (gmath/distance pos target-pos) consts/attack-distance))
      (let [[next-point & path-left] path
            time-cost (/ (gmath/distance pos next-point) speed)]
        (if (< time-cost time-delta)
          (recur (assoc mob :pos next-point :path path-left)
                 (- time-delta time-cost) chars)
          (ccfns/move-toward-pos mob time-delta next-point)))
      mob)))

(defn moved-wrapper [move-fn]
  (fn [char & args]
    (let [pos (:pos char)
          new-char (apply move-fn char args)
          new-pos (:pos new-char)]
      (assoc new-char :moved-this-frame (not (rec== pos new-pos))))))

(def move-player (moved-wrapper move-player*))

(def move-mob (moved-wrapper move-mob*))

(defmacro defmovefn [name type move-fn & args]
  `(defn ~name [{:keys [~@args] :as game-state#}]
     (let [{group# ~type} (group-by (fn [[id# char#]] (:type char#))
                                    (:chars game-state#))
           move-char# #(~move-fn % ~@args)]
       {:new-game-state
        (update-in game-state# [:chars] into (fmap move-char# group#))})))

(defmovefn move-players :player move-player move-time-delta last-move)

(defmovefn move-mobs :mob move-mob move-time-delta chars)

(defn check-if-moved [game-state]
  (when-let [moved (reduce (fn [moved [id char]]
                             (if (:moved-this-frame char)
                               (conj moved id)
                               moved))
                           nil
                           (:chars game-state))]
    {:event {:type :chars-moved :moved-ids moved}}))

(defn check-spawns [{:keys [to-spawn] :as game-state}]
  (let [curr-time (current-time-ms)
        time-to-spawn (fn [[id spawn-time]] (> curr-time spawn-time))
        ids (keys (take-while time-to-spawn to-spawn))]
    (when (seq ids)
      {:event {:type :spawn-ids :ids ids}})))

(defn cooled-down? [{:keys [last-attack delay]}]
  (> (current-time-ms) (+ last-attack (* 1000 delay))))

(defn close-enough? [game-state attacker-id target-id]
  (let [curr-time (current-time-ms)
        get-pos (fn [id] (get-in game-state [:chars id :pos]))
        attacker-pos (get-pos attacker-id)
        target-pos (get-pos target-id)]
    (> consts/attack-distance (gmath/distance attacker-pos target-pos))))

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
                     (get-in game-state [:chars target])
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
          (check-spawns)
          (ccfns/calculate-move-time-delta)
          (move-players)
          (decide-mob-actions)
          (decide-mob-paths)
          (move-mobs)
          (check-if-moved)
          (let-chars-attack))
        {:keys [new-events new-game-state]} (process-events new-game-state
                                                            events)
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
  (-> {:chars {} :player-ids #{}
       :last-move (current-time-ms)}
      (merge (gmap/load-game-map))
      (as-> gs
        (assoc gs :to-spawn (create-to-spawn-queue (:spawns gs))))))

(defn init-server [port]
  (let [game-state (create-game-state)
        stop? (atom false)
        {:keys [net-sys get-msg send-msg]}
        (net/construct-server-net-sys port cc/connect-msg cc/disconnect-msg)
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
