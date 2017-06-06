(ns game.common.input
  (:require
   [clojure.string :as str]
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns])
  (:import
   (com.jme3.input KeyInput MouseInput)
   (com.jme3.input.controls ActionListener KeyTrigger MouseButtonTrigger)))

(defn string->trigger [s]
  (let [[input trigger prefix s]
        (cond
          (.startsWith (.toLowerCase s) "m-")
          [MouseInput MouseButtonTrigger "BUTTON_" (.substring s 2)]
          :else
          [KeyInput KeyTrigger "KEY_" s])
        key-code (-> (.getField input (str prefix (.toUpperCase s)))
                   (.get nil))]
    (eval `(new ~trigger ~key-code))))

(defn create-triggers [key-bindings]
  (map (fn [[name key type]] [name (string->trigger key) type])
       key-bindings))

(defn create-key-state-map [key-bindings]
  (zipmap (map (comp keyword first)
               (filter (fn [[_ _ type]] (= type :hold)) key-bindings))
          (repeat false)))

(deftype InputSystem [event-queue-ref start-fn]
  cc/Lifecycle
  (start [this]
    (start-fn))
  (stop [this])
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue event-queue-ref))
  cc/Updatable
  (update [this args]))

(def modifier-bindings
  [["shift" "lshift" :hold]
   ["ctrl" "lcontrol" :hold]
   ["alt" "lmenu" :hold]])

(defn split-name-and-number [name]
  (if (re-find #"#" name)
    (update (str/split name #"#") 1 #(Integer/parseInt %))
    [name nil]))

(defn init-input-system [input-manager user-key-bindings]
  (let [key-bindings (into user-key-bindings modifier-bindings)
        key-state-atom (atom (create-key-state-map key-bindings))
        event-queue-ref (ref [])
        keyinput-bindings (create-triggers key-bindings)
        hold-listener (reify ActionListener
                        (onAction [this name pressed tpf]
                          (swap! key-state-atom assoc (keyword name) pressed)
                          (ccfns/queue-conj event-queue-ref
                                            {:type :new-key-state
                                             :key-state @key-state-atom})))
        tap-listener (reify ActionListener
                       (onAction [this name pressed tpf]
                         (when pressed
                           (let [[name number] (split-name-and-number name)]
                             (ccfns/queue-conj
                              event-queue-ref {:type (keyword name)
                                               :number number})))))
        filter-fn (fn [type]
                    (into-array
                     (map first (filter #(= type (% 2)) key-bindings))))
        tap-names (filter-fn :tap)
        hold-names (filter-fn :hold)
        start-fn (fn []
                   (doseq [[name trigger _] keyinput-bindings]
                     (.addMapping input-manager name (into-array [trigger])))
                   (.addListener input-manager hold-listener hold-names)
                   (.addListener input-manager tap-listener tap-names))]
    (->InputSystem event-queue-ref start-fn)))
