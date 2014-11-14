(ns game.server.core
  (:require [clojure.data.priority-map :as pm]
            [game.networking.core :as net]
            [clojure.math.numeric-tower :as math]
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [items :as items]
                         [stats :as stats])
            (game.key-value-store [core :as kvs.core]
                                  [protocols :as kvs])
            (game [math :as gmath]
                  [game-map :as gmap]
                  [mobs :as mobs]
                  [constants :as consts])
            (game.server [pathfinding :as pf]
                         [ai :as ai]))
  (:use game.utils))

(defn new-player [username]
  (ccfns/update-stats
    {:name username
     :speed 2
     :pos [1 1]
     :bind-spot [1 1]
     :move-dir [0 0]
     :type :player
     :attacking false
     :hp 100
     :max-hp 100
     :damage 60
     :delay 1
     :last-attack 0
     :level 1
     :exp 0
     :inv (-> (vec (repeat 10 nil))
              (assoc-in [0] {:stats {:armor 20}, :id 0}))
     :gear (zipmap items/gear-slots (repeat nil))}))

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
    (if-let [game-id (@net->game net-id)]
      game-id
      (new-game-id net-id)))
  (defn game-id->net-id [game-id]
    (@game->net game-id)))

(defmulti process-event (fn [game-state event] (:type event)))

(defmethod process-event :default [game-state event])

(defn make-preparation-fn [ks]
  (fn [char]
    (let [prepared (select-keys char ks)
          pos (:pos prepared)]
      (cond-> prepared
        pos (assoc :pos (map float pos))))))

(def char-update-keys [:max-hp :level])

(def keys-about-others
  (concat char-update-keys [:name :speed :pos :type :hp :level]))

(def prepare-char-update (make-preparation-fn char-update-keys))

(def prepare-char-for-owner
  (make-preparation-fn
    (concat keys-about-others [:damage :delay :exp :inv :gear])))

(def prepare-char-for-sending
  (make-preparation-fn keys-about-others))

(def prepare-corpse-for-sending
  (make-preparation-fn [:name :pos :type]))

(defn prepare-chars-for-sending [m]
  (fmap prepare-char-for-sending m))

(defn prepare-corpses-for-sending [m]
  (fmap prepare-corpse-for-sending m))

(defn prepare-for-sending-to [id game-state]
  (let [player (prepare-char-for-owner (get-in game-state [:chars id]))]
    (-> game-state
        (update-in [:chars] prepare-chars-for-sending)
        (update-in [:corpses] prepare-corpses-for-sending)
        (select-keys [:chars :corpses])
        (assoc-in [:chars id] player))))

(defmethod process-event :c-move [game-state {:keys [id pos move-dir]}]
  {:new-game-state
   (-> game-state
       (update-in [:chars id] merge
                  {:recv-pos pos :move-dir (gmath/normalize move-dir)
                   :recv-time (current-time-ms)}))})

(defmethod process-event :c-toggle-attack [game-state {:keys [id]}]
  {:new-game-state (update-in game-state [:chars id :attacking] not)})

(defmethod process-event :c-target [game-state {:keys [id target]}]
  {:new-game-state (assoc-in game-state [:chars id :target] target)})

(defmethod process-event :c-loot-corpse [game-state {:keys [id corpse-id]}]
  (when (ccfns/id-close-enough? game-state id corpse-id consts/loot-distance)
    {:new-game-state (update-in game-state [:corpses corpse-id :looting]
                                #(or (some-> % (conj id)) #{id}))
     :msg [[id] {:type :s-loot :corpse-id corpse-id
                 :drops (get-in game-state [:corpses corpse-id :drops])}]}))

(defn whose-item? [path]
  (path 1))

(defn can-loot? [game-state id path]
  (when-let [corpse-id (whose-item? path)]
    (and (some-> (get-in game-state [:corpses corpse-id :tagged-by]) (= id))
         (ccfns/id-close-enough? game-state id corpse-id
                                 consts/loot-distance))))

(defn update-looting [game-state corpse-id]
  (->> (get-in game-state [:corpses corpse-id :looting])
       (filter #(ccfns/id-close-enough? game-state % corpse-id
                                        consts/loot-distance))
       set))

(defn loot-item [game-state id from-path]
  (let [ngs (ccfns/loot-item game-state from-path [:chars id :inv])
        looting (get-in ngs [:corpses (from-path 1) :looting])]
    {:new-game-state ngs
     :msgs [[[id] {:type :s-loot-item-ok :from-path from-path}]
            [(disj looting id)
             {:type :s-item-looted :path from-path
              :left (:quantity (get-in ngs from-path))}]]}))

(defmethod process-event :c-loot-item [game-state {:keys [id from-path]}]
  (when (can-loot? game-state id from-path)
    (let [corpse-id (from-path 1)
          ngs (assoc-in game-state [:corpses corpse-id :looting]
                        (update-looting game-state corpse-id))]
      (loot-item ngs id from-path))))

(defmethod process-event :c-login [game-state event]
  (let [key-value-store (:kvs game-state)
        {:keys [id username password]} event
        player (or (kvs/load key-value-store username)
                   (new-player username))
        old-players (:player-ids game-state)
        new-game-state (-> game-state
                           (assoc-in [:chars id] player)
                           (update-in [:player-ids] conj id))
        gs-for-entrant (prepare-for-sending-to id new-game-state)]
    {:new-game-state new-game-state
     :msgs [[[id] {:type :s-game-state :game-state gs-for-entrant}]
            [[id] {:type :s-own-id :id id}]
            [old-players
             {:type :s-spawn-player :id id
              :player (prepare-char-for-sending player)}]]}))

(defmethod process-event :c-rearrange-inv [game-state {:keys [id paths]}]
  (let [real-paths (map #(into [:chars id] %) paths)]
    (ccfns/inv-swap game-state real-paths 4 nil {:type :changed-gear :id id})))

(defmethod process-event :c-move-quantity
  [game-state {:keys [id from-path to-path quantity] :as event}]
  (let [[from to] (map #(into [:chars id] %) [from-path to-path])]
    {:new-game-state (ccfns/move-quantity game-state from to quantity)}))

(defmethod process-event :c-destroy-item [game-state {:keys [id path quantity]}]
  (let [sv-path (into [:chars id] path)]
    {:new-game-state (ccfns/destroy-item game-state sv-path quantity)}))

(defmethod process-event :c-quit-looting [game-state {:keys [id ids]}]
  {:new-game-state
   (reduce (fn [gs cid]
             (update-in gs [:corpses cid :looting] disj id))
           game-state
           ids)})

(defn update-player [game-state id]
  (let [ngs (update-in game-state [:chars id] ccfns/update-stats)]
    {:new-game-state ngs
     :msgs [[(:player-ids game-state)
             {:id id
              :type :s-char-update
              :updated (prepare-char-update (get-in ngs [:chars id]))}]]}))

(defmethod process-event :changed-gear [game-state {:keys [id]}]
  (update-player game-state id))

(defmethod process-event :level-up [game-state {:keys [id] :as event}]
  (update-player game-state id))

(defn roll-for-mob [mobs]
  (let [total (apply + (map :rel-chance mobs))]
    (reduce (fn [left mob]
              (let [new-left (- left (:rel-chance mob))]
                (if (neg? new-left)
                  (reduced mob)
                  new-left)))
            (rand total)
            mobs)))

(defn roll-for-drops [drops]
  (for [item drops
        :let [q (or (:quantity item) 1)
              actual (rand-binomial q (:chance item))]
        :when (< 0 actual)]
    {:id (:id item) :quantity actual}))

(defn spawn-mob [spawn-id spawns]
  (let [mob (-> spawn-id spawns :mobs roll-for-mob :mob)
        mob-type (-> mob :type mobs/mobs)
        [min max] (:levels mob)
        level (+ min (rand-int (- (inc max) min)))
        drops (roll-for-drops (:drops mob))
        drops-with-stats (map items/roll-for-stats drops)
        unstacked (vec (mapcat items/unstack drops-with-stats))]
    (assoc mob-type
           :type :mob
           :spawn spawn-id
           :pos (-> spawn-id spawns :pos)
           :move-dir [0 0]
           :last-attack 0
           :max-hp (:hp mob-type)
           :delay 1
           :level 1
           :drops unstacked)))

(defmethod process-event :spawn-ids
  [{:keys [to-spawn spawns] :as game-state} {ids :ids}]
  (let [new-mobs (map #(spawn-mob % spawns) ids)
        new-mobs-map (zipmap (repeatedly new-game-id) new-mobs)
        num-mobs (count new-mobs-map)
        all-players (:player-ids game-state)]
    {:new-game-state
     (-> game-state
         (update-in [:chars] merge new-mobs-map)
         (assoc :to-spawn (call-times num-mobs pop to-spawn)))
     :msg [all-players {:type :s-spawn-mobs
                        :mobs (prepare-chars-for-sending new-mobs-map)}]}))

(defn register-damage [char id damage]
  (let [tag #(or % id)
        set-damage #(+ (or % 0) damage)]
    (-> char
        (update-in [:tagged-by] tag)
        (update-in [:damaged-by id] set-damage))))

(defn process-hit [game-state event]
  (let [{:keys [id target damage last-attack]} event
        register-damage* #(if (ccfns/mob? %)
                            (register-damage % id damage)
                            %)]
    (-> game-state
        (update-in [:chars target :hp] - damage)
        (update-in [:chars target] register-damage*))))

(defmethod process-event :attack [game-state event]
  (let [{:keys [id target damage last-attack hit]} event
        ngs (cond-> game-state
              true (assoc-in [:chars id :last-attack] last-attack)
              hit (process-hit event))]
    {:new-game-state ngs
     :event (when (<= (get-in ngs [:chars target :hp]) 0)
              {:type :death :id target :by id :corpse-id (new-game-id)})
     :msg [(:player-ids game-state)
           {:type :s-attack :target target :damage damage :hit hit}]}))

(defn give-exp [{:keys [level exp] :as char} new-exp]
  (let [exp-sum (+ exp new-exp)
        req-exp (stats/exp-to-level (inc level))]
    (if (> exp-sum req-exp)
      (-> char
          (update-in [:level] inc)
          (assoc-in [:exp] (- exp-sum req-exp)))
      (update-in char [:exp] + new-exp))))

(defn distribute-exp [game-state char]
  (let [{:keys [damaged-by tagged-by]} char
        total-damage (apply + (vals damaged-by))
        tagged-damage (damaged-by tagged-by)
        all-exp (stats/exp-gained (:level char)
                                  (get-in game-state [:chars tagged-by :level]))
        actual-exp (* all-exp (/ tagged-damage total-damage))]
    (update-in game-state [:chars tagged-by]
               give-exp (* consts/exp-bonus-factor actual-exp))))

(defn mob-death [game-state {:keys [id]}]
  (let [mob (get-in game-state [:chars id])
        spawn-id (:spawn mob)
        respawn-time (get-in game-state [:spawns spawn-id :respawn-time])]
    (-> game-state
        (dissoc-in [:chars id])
        (update-in [:to-spawn] conj
                   [spawn-id (+ (current-time-ms) (* 1000 respawn-time))])
        (distribute-exp mob))))

(defn player-death [game-state {:keys [id]}]
  (let [char-set (fn [gs key val] (assoc-in gs [:chars id key] val))
        char-get (fn [key] (get-in game-state [:chars id key]))]
    (-> game-state
        (char-set :pos (char-get :bind-spot))
        (char-set :hp (char-get :max-hp))
        (char-set :target nil)
        (char-set :attacking false))))

(defn make-corpse [char]
  (-> char
      (select-keys [:name :type :drops :pos :tagged-by])
      (assoc :decay-time (+ (current-time-ms) consts/corpse-decay-time))
      (update-in [:name] #(str % "'s corpse"))))

(defn death-msgs [game-state {:keys [id corpse-id]}]
  (let [all-players (:player-ids game-state)
        char (get-in game-state [:chars id])
        corpse (prepare-corpse-for-sending
                 (get-in game-state [:corpses corpse-id]))
        msgs-vec [[all-players {:type :s-char-death :id id}]
                  [all-players {:type :s-spawn-corpse
                                :id corpse-id :corpse corpse}]]]
    (if (ccfns/player? char)
      (conj msgs-vec
            [all-players {:type :s-spawn-player :id id
                          :player (prepare-char-for-sending char)}])
      msgs-vec)))

(defn leveled-up? [old-gs new-gs {:keys [tagged-by] :as killed-char}]
  (let [get-level (fn [gs] (get-in gs [:chars tagged-by :level]))]
    (and (ccfns/mob? killed-char) (> (get-level new-gs) (get-level old-gs)))))

(defmethod process-event :death [game-state {:keys [id corpse-id] :as event}]
  (let [char (get-in game-state [:chars id])
        corpse (make-corpse char)
        new-game-state (-> (if (ccfns/mob? char)
                             (mob-death game-state event)
                             (player-death game-state event))
                           (update-in [:corpses] assoc corpse-id corpse))]
    (conj-some {:new-game-state new-game-state
                :msgs (death-msgs new-game-state event)}
               (when (leveled-up? game-state new-game-state char)
                 {:event {:type :level-up :id (:tagged-by char)}}))))

(defn get-id-and-hp [[id char]]
  [id (:hp char)])

(defmethod process-event :regen-tick
  [{:keys [chars player-ids] :as game-state} event]
  {:msg [player-ids {:type :s-hp-update
                     :id-hp-vecs (map get-id-and-hp chars)}]})

(defmethod process-event :decay-corpses [game-state {:keys [ids]}]
  {:new-game-state
   (update-in game-state [:corpses] #(apply dissoc % ids))
   :msg [(:player-ids game-state) {:type :s-decay-corpses :ids ids}]})

(defmethod process-event :chars-moved [game-state {ids :moved-ids}]
  {:msg [(:player-ids game-state)
         {:type :s-move :positions
          (into {} (for [id ids
                         :let [pos (get-in game-state [:chars id :pos])]
                         :when pos]
                     [id (map float pos)]))}]})

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

(defn check-corpses [{:keys [corpses] :as game-state}]
  (let [curr-time (current-time-ms)
        time-to-decay (fn [[id {:keys [decay-time]}]] (> curr-time decay-time))
        ids (keys (take-while time-to-decay corpses))]
    (when (seq ids)
      {:event {:type :decay-corpses :ids ids}})))

(defn cooled-down? [{:keys [last-attack delay]}]
  (> (current-time-ms) (+ last-attack (* 1000 delay))))

(defn calculate-new-last-attack [{:keys [last-attack delay]}]
  (let [curr-time (current-time-ms)]
    (if (> (+ last-attack (* 1000 (+ delay consts/attack-delay-leeway)))
           curr-time)
      (+ last-attack (* 1000 delay))
      curr-time)))

(defn let-chars-attack [game-state]
  (let [generate-attack-event
        (fn [[id {:keys [target attacking damage last-attack delay] :as char}]]
          (let [target-char (get-in game-state [:chars target])
                event {:id id :type :attack :target target
                       :last-attack (calculate-new-last-attack char)}]
            (when (and attacking target
                       (cooled-down? char)
                       target-char
                       (ccfns/id-close-enough? game-state id target
                                               consts/attack-distance))
              (conj event (if (stats/hit? char target-char)
                            {:hit true
                             :damage (stats/actual-damage char target-char)}
                            {:hit false})))))]
    {:events (remove nil? (map generate-attack-event (:chars game-state)))}))

(defn regen-char [{:keys [hp-regen hp max-hp] :as char}]
  (assoc-in char [:hp] (min max-hp (+ hp (or hp-regen 0)))))

(defn regen-chars [{:keys [last-regen] :as game-state}]
  (let [curr-time (current-time-ms)
        regen-interval (* 1000 consts/regen-interval)]
    (when (> curr-time (+ last-regen regen-interval))
      {:new-game-state
       (-> game-state
           (update-in [:chars] #(fmap regen-char %))
           (update-in [:last-regen] + regen-interval))
       :event {:type :regen-tick}})))

(defn get-network-events [_ net-sys]
  {:events (cc/get-events net-sys)})

(defn make-process-and-send-fn [networking-system]
  (fn [game-state events]
    (let [{:keys [new-game-state new-msgs]}
          (ccfns/process-events process-event game-state events)]
      (cc/update networking-system new-msgs)
      {:new-game-state new-game-state})))

(defn main-update [game-state net-sys]
  (let [hook (make-process-and-send-fn net-sys)
        {:keys [new-game-state events]}
        (ccfns/call-update-fns game-state [] hook
          (get-network-events net-sys)
          (ccfns/calculate-move-time-delta)
          (move-players)
          (ai/decide-mob-actions)
          (ai/decide-mob-paths)
          (move-mobs)
          (check-if-moved)
          (let-chars-attack)
          (regen-chars)
          (check-spawns)
          (check-corpses))]
    new-game-state))

(defrecord Server [net-sys key-value-store game-state stop?]
  cc/Lifecycle
  (start [this]
    (cc/start net-sys)
    (cc/start key-value-store)
    (start-new-thread "server"
      ((fn [game-state]
         (if @stop?
           nil
           (recur (take-at-least-ms 100
                    (main-update game-state net-sys)))))
       game-state))
    this)
  (stop [this]
    (reset! stop? true)
    (cc/stop key-value-store)
    (cc/stop net-sys)
    this))

(defn create-to-spawn-queue [spawns]
  (apply pm/priority-map (interleave (keys spawns) (repeat 0))))

(defn create-game-state []
  (-> {:chars {} :player-ids #{} :corpses (pm/priority-map-keyfn :decay-time)
       :last-move (current-time-ms) :last-regen (current-time-ms)}
      (merge (gmap/load-game-map))
      (as-> gs
        (assoc gs :to-spawn (create-to-spawn-queue (:spawns gs))))))

(defn init-server [port]
  (let [game-state (create-game-state)
        stop? (atom false)
        networking-system (net/init-server-net-sys
                            port net-id->game-id game-id->net-id)
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server networking-system key-value-store
              (assoc game-state :kvs key-value-store) stop?)))
