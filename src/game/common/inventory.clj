(ns game.common.inventory
  (:require
   [game.items :as items]
   [game.utils :refer :all]))

(defn possible-slot? [game-state from to]
  (let [item (get-in game-state from)]
    (items/correct-slot? item to)))

(defn inv-swap
  "If possible, swap the item in from with to. If either of from or to were
   in a gear map (and not just inv), enqueue a :changed-gear event."
  [game-state [from to :as paths] enqueue id]
  (when (and (possible-slot? game-state from to)
             (possible-slot? game-state to from))
    (let [path-len (count (first paths))
          gear-index (- path-len 2)]
      (enqueue (when (some #{:gear} (map #(% gear-index) paths))
                 {:type :changed-gear :id id})))
    (swap-in game-state from to)))

(defn find-first-nil [v]
  (first (keep-indexed (fn [idx value] (if (nil? value) idx)) v)))

(defn divide-into-piles
  ([maxes total] (divide-into-piles maxes total []))
  ([maxes total acc]
   (if (seq maxes)
     (let [curr (first maxes)]
       (if (> total curr)
         (recur (rest maxes) (- total curr) (conj acc curr))
         (conj acc total)))
     (conj acc total))))

(defn stack-loot-dest [inv {:keys [quantity id] :as item}]
  (let [size (:stackable (items/all-info item))
        idx-place (->> (map-indexed vector inv)
                    (filter (fn [[idx item]]
                              (and item
                                   (== id (:id item))
                                   (< (:quantity item) size))))
                    (map (fn [[idx item]] [idx (- size (:quantity item))])))
        distribution (divide-into-piles (map second idx-place) quantity)
        will-use (zip (map first idx-place) distribution)
        need-extra-slot (< (count will-use) (count distribution))
        first-nil (find-first-nil inv)]
    (conj-some {:add-to will-use}
               (when (and need-extra-slot first-nil)
                 {:extra-slot [first-nil (peek distribution)]}))))

(defn loot-stack [game-state from-path to-inv-path]
  (let [{:keys [quantity] :as item} (get-in game-state from-path)
        {:keys [add-to extra-slot]} (stack-loot-dest
                                     (get-in game-state to-inv-path) item)
        [extra-idx extra-n] extra-slot
        add-to-stack (fn [gs [idx n]]
                       (update-in gs (conj to-inv-path idx :quantity) + n))
        ngs (reduce add-to-stack game-state add-to)
        total-looted (+ (reduce + (map second add-to))
                        (if extra-slot extra-n 0))]
    (cond-> (update-in ngs from-path
                       (if (= total-looted quantity)
                         (constantly nil)
                         #(update % :quantity - total-looted)))
      extra-slot (assoc-in (conj to-inv-path extra-idx)
                           (assoc item :quantity extra-n)))))

(defn loot-nonstack [game-state from-path to-inv-path]
  (when-let [to-idx (find-first-nil (get-in game-state to-inv-path))]
    (move-in game-state from-path (conj to-inv-path to-idx))))

(defn loot-item [game-state from-path to-inv-path]
  ((if (:quantity (get-in game-state from-path))
     loot-stack
     loot-nonstack)
   game-state from-path to-inv-path))

(defn ensure-to-stack [game-state from-path to-path]
  (if-not (get-in game-state to-path)
    (let [fake-item (assoc (get-in game-state from-path)
                           :quantity 0)]
      (assoc-in game-state to-path fake-item))
    game-state))

(defn do-move-quantity [game-state from-path to-path quantity]
  (let [game-state (ensure-to-stack game-state from-path to-path)
        [from-q-path to-q-path] (map #(conj % :quantity) [from-path to-path])
        [from-q to-q] (map #(get-in game-state %) [from-q-path to-q-path])
        max-stack (:stackable (items/all-info (get-in game-state to-path)))
        moved-q (min quantity from-q (- max-stack to-q))
        ngs (update-in game-state to-q-path + moved-q)]
    (if (<= from-q moved-q)
      (dissoc-in ngs from-path)
      (update-in ngs from-q-path - moved-q))))

(defn move-quantity [game-state from-path to-path quantity]
  (when (and (possible-slot? game-state from-path to-path)
             (not= from-path to-path))
    (do-move-quantity game-state from-path to-path quantity)))

(defn destroy-item [game-state path destroy-quantity]
  (if destroy-quantity
    (let [quant-path (conj path :quantity)
          quantity (get-in game-state quant-path)]
      (if (> quantity destroy-quantity)
        (update-in game-state quant-path - destroy-quantity)
        (dissoc-in game-state path)))
    (dissoc-in game-state path)))
