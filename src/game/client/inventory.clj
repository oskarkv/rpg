(ns game.client.inventory
  (:require
   [game.client.base :as b]
   [game.common.inventory :as inv]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.items :as items]
   [game.stats :as stats]
   [game.utils :refer :all]))

(defmethod b/process-event :s-loot-item-ok [game-state {:keys [from-path]}]
  (inv/loot-item game-state from-path [:inv]))

(defmethod b/process-event :s-item-looted [game-state {:keys [from-path by left]}]
  (if left
    (assoc-in game-state (conj from-path :quantity) left)
    (dissoc-in game-state from-path)))

(defn update-own-stats [{:keys [own-id gear] :as game-state}]
  (update-in game-state [:chars own-id]
             #(-> %
                (assoc :gear gear)
                stats/update-stats
                (dissoc :gear))))

(defmethod b/process-event :changed-gear [game-state event]
  (update-own-stats game-state))

(defn init-destroy-item [game-state]
  (assoc game-state :destroying-item true))

(defn destroy-item [game-state]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state]
    (b/enqueue-events {:type :c-destroy-item :path on-mouse
                       :quantity on-mouse-quantity})
    (-> (inv/destroy-item game-state on-mouse on-mouse-quantity)
      (dissoc :on-mouse :on-mouse-quantity))))

(defmethod b/process-event :destroy-item [game-state {:keys [destroy]}]
  (dissoc
   (if destroy
     (destroy-item game-state)
     game-state)
   :destroying-item))

(defmethod b/process-event :open-inv [game-state _]
  (update game-state :inv-open? not))

(defn my-inv? [path]
  (= :inv (first path)))

(defn my-gear? [path]
  (= :gear (first path)))

(defn my-stuff? [path]
  (or (my-inv? path) (my-gear? path)))

(defn drop-stack-onto-stack [game-state path]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state]
    (b/enqueue-events {:type :move-quantity :from-path on-mouse :to-path path
                       :quantity on-mouse-quantity})))

(defn swap-places [game-state path]
  (b/enqueue-events {:type :inv-swap :paths [(:on-mouse game-state) path]}))

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
  (b/enqueue-events {:type :c-loot-item :from-path path}))

(defn get-prioritized-slot
  "Finds a slot the gear map for item. Prioritizes empty slots."
  [{:keys [gear] :as game-state} item]
  (let [slots (items/expand-slot (:slot item))
        empty-slots (remove #(% gear) slots)]
    (first (concat empty-slots slots))))

(defn equip-item [game-state path]
  (when-lets [item (get-in game-state path)
              slot (get-prioritized-slot game-state item)]
    (b/enqueue-events {:type :inv-swap :paths [[:gear slot] path]})))

(defn activate-item [game-state path]
  (condp #(= %1 (first %2)) path
    :corpses (loot-item game-state path)
    :inv (equip-item game-state path)
    nil))

(defmethod b/process-event :inv-click [game-state {:keys [path button pressed]}]
  (->$ (cond
         (and pressed (= button consts/mouse-left) (#{:inv :gear} (path 0)))
         (pick-up-or-drop-item game-state path)
         (and pressed (= button consts/mouse-right))
         (activate-item game-state path))
    (if $
      (dissoc $ :destroying-item)
      (dissoc game-state :destroying-item))))

(defmethod b/process-event :inv-swap [game-state {paths :paths :as event}]
  (when (every? my-stuff? paths)
    (b/enqueue-events {:type :c-rearrange-inv :paths paths})
    (inv/inv-swap game-state paths b/enqueue-events nil)))

(defmethod b/process-event :move-quantity
  [game-state {:keys [from-path to-path quantity] :as event}]
  (when (every? my-stuff? [from-path to-path])
    (b/enqueue-events (assoc event :type :c-move-quantity))
    (inv/move-quantity game-state from-path to-path quantity)))
