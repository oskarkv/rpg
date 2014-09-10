(ns game.client.hud
  (:use game.utils)
  (:require (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [items :as items])
            [game.constants :as consts]
            [game.client.hudlib :as lib]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]))

(def depths (zipmap [:mouse-slot :tooltip]
                    (map #(* 10 %) (drop 1 (range)))))

(defn create-stack-indicator [screen item]
  (doto (lib/create-text-element screen nil)
    (lib/set-text (str (:quantity item)))
    (lib/set-alignment :bottom)
    (lib/set-alignment :right)
    (lib/set-font-size 16)))

(defn create-slot [screen item size clickable]
  (let [slot (lib/create-element
               screen
               (merge
                 (if item
                   {:texture-name (:icon (items/all-info item)) :tooltip "item"}
                   {:texture-name "inv_slot.png"})
                 {:size size :clickable clickable}))]
    (if (:quantity item)
      (lib/add-child
        slot (lib/set-size (create-stack-indicator screen item) size))
      slot)))

(defn slot-positions [n cols size gap]
  (let [padded-n (+ n (- cols (mod n cols)))
        rows (/ padded-n cols)
        calc-pos #(+ gap (* % (+ size gap)))]
    (for [y (range rows) x (range cols)]
      (mapv calc-pos [x y]))))

(defn create-inventory-element [screen items cols size gap]
  (let [n (count items)
        slots (map #(create-slot screen % size true) items)
        positions (slot-positions n cols size gap)
        container (doto (lib/create-window screen nil)
                    (lib/set-text "nu da something hehehehe?")
                    (lib/set-color [0.2 0.5 0.2 0.5])
                    (#(lib/set-color (:header %) [0.2 0.5 0.2 0.8])))]
    (dorun (map #(lib/set-position %1 %2) slots positions))
    (dorun (map #(lib/add-child container %) slots))
    (lib/auto-size container gap)
    (lib/add-child screen container)
    {:element container :slots slots}))

(defn path->pos [path hud-state]
  (let [positions (:positions hud-state)]
    (or ((first path) positions) (:other positions))))

(defn get-map-order [game-state path]
  (if (= path [:gear])
    items/gear-slots-vector
    (keys (get-in game-state path))))

(defn items-paths-pos-cols [path game-state hud-state]
  (let [items (get-in game-state path)
        add-to-path (fn [endings] (map #(conj path %) endings))
        pos (path->pos path hud-state)]
    (if (map? items)
      (let [order (get-map-order game-state path)]
        [(map #(% items) order) (add-to-path order) pos 4])
      [items (add-to-path (range (count items))) pos 2])))

(defn create-element-maps [element slots path item-paths]
  (let [suffixes (map peek item-paths)]
    {:invs {path element}
     :slot->path (zipmap slots item-paths)
     :path->slot {path (zipmap suffixes slots)}}))

(defn create-inventory [game-state screen hud-state path]
  (let [[items paths pos cols] (items-paths-pos-cols path game-state hud-state)
        {:keys [size gap]} hud-state
        {:keys [element slots]} (create-inventory-element
                                  screen items cols size gap)]
    (lib/set-position element pos)
    (create-element-maps element slots path paths)))

(defn create-inventories [hud-state game-state screen new-paths]
  (let [maps (map #(create-inventory game-state screen hud-state %) new-paths)]
    (merge-with merge hud-state (apply merge-with merge maps))))

(defn clean-slot-maps [{:keys [path->slot slot->path] :as hud-state} gone-paths]
  (let [gone-slots (mapcat (comp vals path->slot) gone-paths)
        remove-from (fn [m k objects]
                      (update-in m [k] #(apply dissoc % objects)))]
    (-> hud-state
        (remove-from :invs gone-paths)
        (remove-from :path->slot gone-paths)
        (remove-from :slot-path gone-slots))))

(defn remove-inventories [hud-state game-state screen gone-paths]
  (dorun (map #(lib/remove-child screen %)
              (map #(get-in hud-state [:invs %]) gone-paths)))
  (clean-slot-maps hud-state gone-paths))

(defn open-inventories [{:keys [looting inv-open?] :as game-state}]
  (set (cond-> (map #(vector :corpses % :drops) looting)
         inv-open? (conj [:inv] [:gear]))))

(defn existing-inventories [hud-state]
  (set (keys (:invs hud-state))))

(defn new-and-gone-inventories [hud-state game-state]
  (let [open (open-inventories game-state)
        existing (existing-inventories hud-state)]
    [(set/difference open existing)
     (set/difference existing open)]))

(defn update-inventories [hud-state game-state screen]
  (let [needs-update (set/intersection (:needs-update hud-state)
                                       (existing-inventories hud-state))
        [news gones] (map #(into needs-update %)
                          (new-and-gone-inventories hud-state game-state))]
    (-> hud-state
        (remove-inventories game-state screen gones)
        (create-inventories game-state screen news)
        (assoc-in [:needs-update] #{}))))

(defn update-mouse-slot-pos [mouse-slot screen size]
  (->> screen lib/get-mouse-pos (map #(+ % (/ size -2)))
       (lib/set-position mouse-slot)))

(defn update-mouse-slot-content [mouse-slot screen game-state size]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state
        item (cond-> (get-in game-state on-mouse)
               on-mouse-quantity (assoc :quantity on-mouse-quantity))]
    (if on-mouse
      (when (empty? (lib/get-children mouse-slot))
        (->> (create-slot screen item size false)
             (lib/add-child mouse-slot)))
      (lib/remove-all-children mouse-slot))))

(defn update-mouse-slot [mouse-slot screen game-state size]
  (update-mouse-slot-pos mouse-slot screen size)
  (update-mouse-slot-content mouse-slot screen game-state size))

(defmulti process-event (fn [hud-state event] (:type event)))

(defn add-needs-update [hud-state paths]
  (update-in hud-state [:needs-update] into paths))

(defmethod process-event :c-rearrange-inv [hud-state {:keys [paths]}]
  (add-needs-update hud-state (map pop paths)))

(defmethod process-event :s-loot-item-ok [hud-state {:keys [from-path to-idx]}]
  (add-needs-update hud-state [(pop from-path) [:inv]]))

(defmethod process-event :s-item-looted [hud-state {:keys [from-path by]}]
  (add-needs-update hud-state [(pop from-path)]))

(defmethod process-event :c-move-quantity
  [hud-state {:keys [from-path to-path]}]
  (add-needs-update hud-state (map pop [from-path to-path])))

(defmethod process-event :default [hud-state event]
  hud-state)

(defn process-events [hud-state events]
  (reduce (fn [hud-state event]
            (process-event hud-state event))
          hud-state
          events))

(defn position-tooltip-text-in-slot [slot text]
  (let [margin consts/tooltip-margin
        size (lib/get-size text)
        add-margin #(+ (* 2 margin) %)]
    (doto slot
      (lib/set-size (map add-margin size))
      (lib/add-child (doto text (lib/set-position [margin margin]))))))

(defn create-tooltip-element [screen string]
  (let [text (doto (lib/create-text-element screen nil)
               (lib/set-text string)
               (lib/auto-size))
        slot (doto (lib/create-element screen nil)
               (lib/set-color [0.2 0.2 0.2 0.5])
               (lib/set-depth (:tooltip depths)))]
    (position-tooltip-text-in-slot slot text)))

(defn update-tooltip-element [hud-state screen]
  (let [{:keys [tooltip-source tooltip]} hud-state
        target-element (lib/get-element-under-cursor screen)]
    (when (not= target-element tooltip-source)
      (let [new-tooltip (when-let [text (and target-element
                                             (lib/get-tooltip target-element))]
                          (create-tooltip-element screen text))]
        (some->> tooltip (lib/remove-child screen))
        (some->> new-tooltip (lib/add-child screen))
        {:tooltip new-tooltip
         :tooltip-source target-element}))))

(defn update-tooltip [hud-state screen]
  (let [new-tooltip (update-tooltip-element hud-state screen)
        {:keys [tooltip-source tooltip]} (or new-tooltip hud-state)]
    (some-> tooltip (lib/set-position (lib/get-mouse-pos screen)))
    (if new-tooltip
      (merge hud-state new-tooltip)
      hud-state)))

(defn create-hp-text [char]
  (apply format "HP: %d/%d" (map math/round [(:hp char) (:max-hp char)])))

(defn update-hp [label char]
  (lib/set-text label (str (:name char) "\n" (create-hp-text char))))

(defn update-hp-bars [game-state self-label target-label]
  (let [{:keys [own-id chars]} game-state
        self (chars own-id)
        target-id (:target self)
        target (chars target-id)]
    (if target
      (update-hp target-label target)
      (lib/set-text target-label ""))
    (update-hp self-label self)))

(deftype HudSystem [gui-node hud-state-atom event-queue enqueue
                    screen self-label target-label]
  cc/Lifecycle
  (start [this]
    (cc/start screen))
  (stop [this]
    (cc/stop screen))
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue event-queue))
  cc/Updatable
  (update [this {:keys [game-state events]}]
    (let [{:keys [mouse-slot size]} @hud-state-atom]
      (update-hp-bars game-state self-label target-label)
      (update-mouse-slot mouse-slot screen game-state size)
      (dorun (map enqueue (cc/get-events screen)))
      (swap! hud-state-atom
             #(-> %
                  (process-events events)
                  (update-inventories game-state screen)
                  (update-tooltip screen))))))

(defn init-hud-system [app]
  (let [gui-node (.getGuiNode app)
        screen (lib/create-screen app)
        mouse-slot (doto (lib/create-container [0 0])
                     (lib/set-depth (:mouse-slot depths)))
        hud-state-atom (atom {:size consts/icon-size :gap consts/icon-gap
                              :needs-update #{}
                              :slot->path {} :path->slot {}
                              :invs {} :mouse-slot mouse-slot
                              :positions {:inv [700 100]
                                          :gear [800 100]
                                          :other [100 100]}})
        path->slot-fn (fn [path]
                        (let [path->slot (:path->slot @hud-state-atom)
                              prefix (pop path)
                              suffix (peek path)]
                          (some-> (path->slot prefix) (get suffix))))
        event-queue (ref [])
        enqueue-event (fn [event] (ccfns/queue-conj event-queue event))
        lookup (fn [slot] (get-in @hud-state-atom [:slot->path slot]))
        enqueue (fn [{:keys [element button pressed]}]
                  (enqueue-event {:type :hud-click :path (lookup element)
                                  :button button :pressed pressed}))
        pw consts/portrait-width
        ph consts/portrait-height
        ry consts/resolution-y
        ch consts/chat-height
        cw consts/chat-width
        gap consts/icon-gap
        self-label (lib/create-text-element screen {:pos gap :size [pw ph]})
        target-label (lib/create-text-element
                       screen {:pos [(+ pw (* 2 gap)) gap] :size [pw ph]})]
    (mapv lib/set-font-size [self-label target-label] (repeat 24))
    (swap! hud-state-atom assoc :path->slot-fn path->slot-fn)
    (lib/add-child screen mouse-slot)
    (lib/add-child screen self-label)
    (lib/add-child screen target-label)
    (->HudSystem gui-node hud-state-atom event-queue enqueue screen
                 self-label target-label)))
