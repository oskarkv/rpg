(ns game.server.combat
  (:require
   [game.common.core-functions :as ccfns]
   [game.stats :as stats]
   [game.constants :as const]
   [game.server.base :as b]
   [game.utils :refer :all]))

(defn pool-max-key [pool-key]
  (keyword (str "max-" (name pool-key))))

(defn pool-regen-key [pool-key]
  (keyword (str (name pool-key) "-regen")))

(defmethod b/process-event :c-toggle-attack [game-state {:keys [id]}]
  (update-in game-state [:chars id :attacking] not))

(defmethod b/process-event :c-target [game-state {:keys [id target]}]
  (assoc-in game-state [:chars id :target] target))

(defn give-exp [{:keys [level exp] :as char} new-exp]
  (let [exp-sum (+ exp new-exp)
        req-exp (stats/exp-to-level (inc level))]
    (if (> exp-sum req-exp)
      (-> char
        (update :level inc)
        (assoc :exp (- exp-sum req-exp)))
      (update char :exp + new-exp))))

(defn distribute-exp [game-state char]
  (let [{:keys [damaged-by tagged-by]} char
        total-damage (apply + (vals damaged-by))
        tagged-damage (damaged-by tagged-by)
        all-exp (stats/exp-gained (:level char)
                                  (get-in game-state [:chars tagged-by :level]))
        actual-exp (* all-exp (/ tagged-damage total-damage))]
    (update-in game-state [:chars tagged-by]
               give-exp (* const/exp-bonus-factor actual-exp))))

(defn mob-death [game-state {:keys [id]}]
  (let [mob (get-in game-state [:chars id])
        spawn-id (:spawn mob)
        respawn-time (get-in game-state [:spawns spawn-id :respawn-time])]
    (-> game-state
      (dissoc-in [:chars id])
      (update :to-spawn conj
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
    (assoc :decay-time (+ (current-time-ms) const/corpse-decay-time))
    (update :name #(str % "'s corpse"))))

(defn death-msgs [game-state {:keys [id corpse-id]}]
  (let [all-players (:player-ids game-state)
        char (get-in game-state [:chars id])
        corpse (b/prepare-corpse-for-sending
                (get-in game-state [:corpses corpse-id]))
        msgs-vec [[all-players {:type :s-char-death :id id}]
                  [all-players {:type :s-spawn-corpse
                                :id corpse-id :corpse corpse}]]]
    (if (ccfns/player? char)
      (conj msgs-vec
            [all-players {:type :s-spawn-player :id id
                          :player (b/prepare-char-for-sending char)}])
      msgs-vec)))

(defn leveled-up? [old-gs new-gs {:keys [tagged-by] :as killed-char}]
  (let [get-level (fn [gs] (get-in gs [:chars tagged-by :level]))]
    (and (ccfns/mob? killed-char) (> (get-level new-gs) (get-level old-gs)))))

(defmethod b/process-event :death [game-state {:keys [id corpse-id] :as event}]
  (let [char (get-in game-state [:chars id])
        corpse (make-corpse char)
        ngs (-> (if (ccfns/mob? char)
                  (mob-death game-state event)
                  (player-death game-state event))
              (update :corpses assoc corpse-id corpse))]
    (apply b/enqueue-msgs (death-msgs ngs event))
    (when (leveled-up? game-state ngs char)
      (b/enqueue-events {:type :level-up :id (:tagged-by char)}))
    ngs))

(defmethod b/process-event :level-up [game-state {:keys [id] :as event}]
  (b/update-player game-state id))

(defn register-damage [char id damage]
  (let [tag #(or % id)
        set-damage #(+ (or % 0) damage)]
    (-> char
      (update :tagged-by tag)
      (update-in [:damaged-by id] set-damage))))

(defn process-hit [game-state event]
  (let [{:keys [id target damage]} event
        register-damage* #(if (ccfns/mob? %)
                            (register-damage % id damage)
                            %)]
    (-> game-state
      (update-in [:chars target :hp] - damage)
      (update-in [:chars target] register-damage*))))

(defmethod b/process-event :attack [game-state event]
  (let [{:keys [id target damage last-attack hit]} event
        ngs (cond-> game-state
              true (assoc-in [:chars id :last-attack] last-attack)
              hit (process-hit event))]
    (b/enqueue-events
     (when (<= (get-in ngs [:chars target :hp]) 0)
       {:type :death :id target :by id :corpse-id (b/new-game-id)}))
    (b/enqueue-msgs [(:player-ids game-state)
                     {:type :s-attack :target target :damage damage :hit hit}])
    ngs))

(defmethod b/process-event :regen-tick [{:keys [chars player-ids]} event]
  (b/enqueue-msgs
   [player-ids
    {:type :s-regen-tick :update-map
     (reduce
      (fn [m [id c]]
        (let [updated
              (into {} (for [k const/regen-pools
                             :when (some-> (pool-regen-key k) c pos?)]
                         [k (k c)]))]
          (if (empty? updated)
            m
            (assoc m id updated))))
      {}
      chars)}]))

(defn cooled-down? [{:keys [last-attack delay]}]
  (> (current-time-ms) (+ last-attack (* 1000 delay))))

(defn calculate-new-last-attack [{:keys [last-attack delay]}]
  (let [curr-time (current-time-ms)]
    (if (> (+ last-attack (* 1000 (+ delay const/attack-delay-leeway)))
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
                                               const/attack-distance))
              (conj event (if (stats/hit? char target-char)
                            {:hit true
                             :damage (stats/actual-damage
                                      char target-char (:damage char))}
                            {:hit false})))))]
    (apply b/enqueue-events (map generate-attack-event (:chars game-state)))))

(defn amount-to-give [char pool-key amount]
  (min amount (- (char (pool-max-key pool-key)) (char pool-key))))

(defn give-char [char pool-key amount]
  (update char pool-key + (amount-to-give char pool-key amount)))

(defn regen-pool [char pool-key]
  (give-char char pool-key (or (char (pool-regen-key pool-key)) 0)))

(defn regen-char [char]
  (reduce (fn [c k]
            (if (k c) (regen-pool c k) c))
          char const/regen-pools))

(defn regen-chars [{:keys [last-regen] :as game-state}]
  (let [curr-time (current-time-ms)
        regen-interval (* 1000 const/regen-interval)]
    (when (> curr-time (+ last-regen regen-interval))
      (b/enqueue-events {:type :regen-tick})
      (-> game-state
        (update :chars #(fmap regen-char %))
        (update :last-regen + regen-interval)))))
