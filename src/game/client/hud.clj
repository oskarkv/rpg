(ns game.client.hud
  (:use game.utils)
  (:require (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [items :as items])
            [game.constants :as consts]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:import (tonegod.gui.core Screen Screen$EventCheckType Element)
           (tonegod.gui.controls.text Label)
           (tonegod.gui.listeners MouseEventListener)
           (com.jme3.math Vector2f Vector4f ColorRGBA)
           (com.jme3.font BitmapText BitmapFont$VAlign BitmapFont$Align
                          LineWrapMode)
           (tonegod.gui.controls.extras ChatBox)))

(def size 40)

(def gap 2)

(defn create-element
  ([screen id [x y] [w h] icon-path]
   (proxy [Element MouseEventListener]
     [screen id (Vector2f. x y) (Vector2f. w h) Vector4f/ZERO icon-path]
     (onMouseEvent [e])))
  ([screen id pos dims icon-path {:keys [click-fn tooltip]}]
   (cond-> (create-element screen id pos dims icon-path)
     click-fn (update-proxy {"onMouseEvent" click-fn})
     tooltip (doto (.setToolTipText tooltip)))))

(defn create-item [screen size enqueue tooltip]
  (create-element
    screen (str "item " (ccfns/get-new-id))
    [0 0] [size size] "fireball.png"
    {:click-fn
     (fn [this e]
       (some-> this .getParent (.onMouseEvent e)))
     :tooltip tooltip}))

(defn create-items [items screen size enqueue]
  (map #(when % (create-item screen size enqueue nil))
       items))

(defn create-slot [screen x y size enqueue]
  (create-element
    screen (str "slot " x ", " y " " (ccfns/get-new-id)) [x y] [size size]
    (-> screen (.getStyle "Window") (.getString "invSlot"))
    {:click-fn (fn [this e]
                 (enqueue this (.getButtonIndex e) (.isPressed e)))}))

(defn create-slots [screen n cols enqueue]
  (let [padded-n (+ n (- cols (mod n cols)))
        rows (/ padded-n cols)
        calc-pos #(+ gap (* % (+ size gap)))
        slot-posses (for [y (range rows) x (range cols)]
                      (mapv calc-pos [x y]))]
    (map (fn [[x y]] (create-slot screen x y size enqueue))
         (take n slot-posses))))

(defn create-slot-maps [slots paths]
  (let [prefix (pop (first paths))
        suffixes (map peek paths)]
    {:slot->path (zipmap slots paths)
     :path->slot {prefix (zipmap suffixes slots)}}))

(defn create-inventory
  [screen items cols paths pos enqueue]
  (assert (= (count items) (count paths)))
  (let [path (butlast (first paths))
        n (count items)
        slots (create-slots screen n cols enqueue)
        items (create-items items screen size enqueue)
        container (doto (create-element screen (str "inv of " path)
                                        pos [0 0] nil)
                    .setAsContainerOnly)]
    (.addElement screen container)
    (dorun (map #(.addChild container %) slots))
    (dorun (map #(when %2 (.addChild % %2)) slots items))
    {:inv container :new-slots slots}))

(defn open-inventories [{:keys [own-id looting inv-open?] :as game-state}]
  (cond-> (map #(vector :corpses % :drops) looting)
    inv-open? (conj [:inv] [:gear])))

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

(defn create-new-inventories [game-state hud-state screen enqueue]
  (let [{:keys [inv-pos loot-pos invs]} hud-state
        open-invs (open-inventories game-state)
        new-paths (remove invs open-invs)
        ippcs (map #(items-paths-pos-cols % game-state hud-state) new-paths)
        new-invs (map (fn [[items paths pos cols]]
                        (let [{:keys [inv new-slots]}
                              (create-inventory screen items cols
                                                paths pos enqueue)]
                          (assoc (create-slot-maps new-slots paths)
                                 :inv inv)))
                      ippcs)
        ids->invs (zipmap new-paths (map :inv new-invs))]
    (-> hud-state
        (update-in [:invs] merge ids->invs)
        (update-in [:slot->path] #(apply merge % (map :slot->path new-invs)))
        (update-in [:path->slot] #(apply merge % (map :path->slot new-invs))))))

(defn remove-inventories [game-state hud-state screen]
  (let [gone-paths (set/difference (set (keys (:invs hud-state)))
                                   (set (open-inventories game-state)))]
    (dorun
      (map (fn [path] (.removeElement screen (get-in hud-state [:invs path])))
           gone-paths))
    (update-in hud-state [:invs] #(apply dissoc % gone-paths))))

(defn update-inventories [hud-state game-state screen enqueue]
  (let [new-hs (create-new-inventories game-state hud-state screen enqueue)]
    (remove-inventories game-state new-hs screen)))

(defn create-mouse-slot [screen]
  (doto (create-element screen "mouse slot" [0 0] [size size] nil)
    .setAsContainerOnly
    (.setIgnoreMouse true)))

(defn update-mouse-slot-pos [mouse-slot screen]
  (.setPosition mouse-slot (.add (.getMouseXY screen)
                                 (Vector2f. (/ size -2) (/ size -2)))))

(defn update-mouse-slot-content [mouse-slot screen game-state]
  (if-let [on-mouse (:on-mouse game-state)]
    (when (empty? (.getElements mouse-slot))
      (let [sizev [size size]
            posv (mapv #(/ % -2) sizev)]
        (.addChild mouse-slot
                   (doto (create-element screen (str "mouse item "
                                                     (ccfns/get-new-id))
                                         [0 0] sizev "fireball.png")
                     (.setIsGlobalModal true)
                     (.setIsModal true)
                     (.setIgnoreMouse true)
                     (.move 0 0 20)))))
    (.removeAllChildren mouse-slot)))

(defn update-mouse-slot [mouse-slot screen game-state]
  (update-mouse-slot-pos mouse-slot screen)
  (update-mouse-slot-content mouse-slot screen game-state))

(defn create-hp-text [char]
  (apply format "HP: %d/%d" (map math/round [(:hp char) (:max-hp char)])))

(defn update-hp [label char]
  (.setText label (str (:name char) "\n" (create-hp-text char))))

(defn update-hp-bars [game-state self-label target-label]
  (let [{:keys [own-id chars]} game-state
        self (chars own-id)
        target-id (:target self)
        target (chars target-id)]
    (if target
      (update-hp target-label target)
      (.setText target-label ""))
    (update-hp self-label self)))

(defmulti process-event (fn [hud-state event] (:type event)))

(defn get-slot-child [slot]
  (some-> slot .getElements first))

(defn swap-slots-contents [{:keys [path->slot-fn] :as hud-state} from to]
  (let [from (path->slot-fn from)
        to (path->slot-fn to)
        transfer (fn [from-slot to-slot child]
                   (some-> from-slot (.removeChild child))
                   (some-> to-slot (.addChild child)))
        to-child (get-slot-child to)
        from-child (get-slot-child from)]
    (when from-child (transfer from to from-child))
    (when to-child (transfer to from to-child)))
  hud-state)

(defmethod process-event :c-rearrange-inv [hud-state {[from to] :paths}]
  (swap-slots-contents hud-state from to))

(defmethod process-event :s-loot-item-ok [hud-state {:keys [from-path to-idx]}]
  (swap-slots-contents hud-state from-path [:inv to-idx]))

(defmethod process-event :s-item-looted [hud-state {:keys [from-path by]}]
  (let [slot (-> hud-state :path->slot-fn (get from-path))]
    (.removeChild slot (get-slot-child slot)))
  hud-state)

(defmethod process-event :default [hud-state event]
  hud-state)

(defn process-events [hud-state events]
  (reduce (fn [hud-state event]
            (process-event hud-state event))
          hud-state
          events))

(defn clean-slot-maps
  [{:keys [path->slot slot->path] :as hud-state} game-state]
  (let [invs (open-inventories game-state)
        gone (keys (reduce dissoc path->slot invs))
        gone-slots (mapcat (comp vals path->slot) gone)]
    (-> hud-state
        (assoc :path->slot (reduce dissoc path->slot gone))
        (assoc :slot->path (reduce dissoc slot->path gone-slots)))))

(defn position-tooltip-text-in-slot [slot text]
  (let [margin consts/tooltip-margin
        x (.getWidth text)
        y (.getHeight text)
        add-margin #(+ (* 2 margin) %)]
    (doto slot
      (.setDimensions (add-margin x) (add-margin y))
      (.addChild (doto text (.setPosition margin margin))))))

(defn create-tooltip-element [screen text]
  (let [create-element* #(create-element
                           screen (str % (ccfns/get-new-id)) [0 0] [0 0] nil)
        text (doto (create-element* "tooltip text")
               (.setText text)
               (.setIgnoreMouse true))
        slot (doto (create-element* "tooltip slot")
               (-> .getGeometry .getMaterial
                   (.setColor "Color" (ColorRGBA. 0.2 0.2 0.2 0.8)))
               (.setIgnoreMouse true))
        bt (doto (.getTextElement text) (.setBox nil))
        tw (.getLineWidth bt)
        th (.getHeight bt)]
    (position-tooltip-text-in-slot slot (doto text (.setDimensions tw th)))))

(defn determine-tooltip-location [screen element]
  (let [v2f (.getMouseXY screen)]
    [(.x v2f) (.y v2f)]))

(defn get-target-element [screen]
  (let [mouse (.getMouseXY screen)
        x (.x mouse) y (.y mouse)]
    (.getContactElement screen x y Screen$EventCheckType/MouseFocus)))

(defn update-tooltip-element [hud-state screen]
  (let [{:keys [tooltip-source tooltip]} hud-state
        target-element (get-target-element screen)]
    (when (not= target-element tooltip-source)
      (let [new-tooltip (when (and target-element
                                   (.getToolTipText target-element))
                          (create-tooltip-element
                            screen (.getToolTipText target-element)))]
        (when tooltip (.removeElement screen tooltip))
        (when new-tooltip (.addElement screen new-tooltip))
        {:tooltip new-tooltip
         :tooltip-source target-element}))))

(defn update-tooltip [hud-state screen]
  (let [new-tooltip (update-tooltip-element hud-state screen)
        {:keys [tooltip-source tooltip]} (or new-tooltip hud-state)
        [x y] (determine-tooltip-location screen tooltip)]
    (some-> tooltip (.moveTo x y))
    (if new-tooltip
      (merge hud-state new-tooltip)
      hud-state)))

(deftype HudSystem [gui-node hud-state-atom event-queue enqueue
                    screen chat-box self-label target-label]
  cc/Lifecycle
  (start [this]
    (.addControl gui-node screen))
  (stop [this])
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue event-queue))
  cc/Updatable
  (update [this {:keys [game-state events]}]
    (update-hp-bars game-state self-label target-label)
    (update-mouse-slot (:mouse-slot @hud-state-atom) screen game-state)
    (swap! hud-state-atom
           #(-> %
                (update-inventories game-state screen enqueue)
                (process-events events)
                (update-tooltip screen)
                (clean-slot-maps game-state)))))

(defn init-hud-system [app]
  (let [gui-node (.getGuiNode app)
        screen (Screen. app "gamedef/style_map.gui.xml")
        mouse-slot (create-mouse-slot screen)
        hud-state-atom (atom {:slot->path {} :path->slot {}
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
        enqueue (fn [slot button pressed]
                  (enqueue-event {:type :hud-click :path (lookup slot)
                                  :button button :pressed pressed}))
        pw consts/portrait-width
        ph consts/portrait-height
        ry consts/resolution-y
        ch consts/chat-height
        cw consts/chat-width
        psize (Vector2f. pw ph)
        self-label (Label. screen "self" (Vector2f. gap gap) psize)
        target-label (Label. screen "target" (Vector2f. (+ pw (* 2 gap)) gap)
                             psize)
        chat-box (proxy [ChatBox]
                   [screen "chat"
                    (Vector2f. gap (- ry ch gap))
                    (Vector2f. cw ch)]
                   (onSendMsg [msg]
                     (.receiveMsg this msg)))]
    (swap! hud-state-atom assoc :path->slot-fn path->slot-fn)
    (.setTextVAlign self-label BitmapFont$VAlign/Top)
    (.setTextVAlign target-label BitmapFont$VAlign/Top)
    (.addElement screen mouse-slot)
    (.addElement screen self-label)
    (.addElement screen target-label)
    (.addElement screen chat-box)
    ;(create-inventory screen (range 7) 2 1 enqueue)
    (->HudSystem gui-node hud-state-atom event-queue enqueue screen
                 chat-box self-label target-label)))

