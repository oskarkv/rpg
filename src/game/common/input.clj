(ns game.common.input
  (:use game.utils)
  (:import (com.jme3.input
             KeyInput
             MouseInput)
           (com.jme3.input.controls
             KeyTrigger
             MouseButtonTrigger
             ActionListener)))

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
  (-> (zipmap (map (comp keyword first)
                   (filter (fn [[_ _ type]] (= type :hold)) key-bindings))
              (repeat false))
      (assoc :taps [])))

(defn empty-taps [key-state-atom]
  (swap! key-state-atom assoc :taps []))

(defn start-input [input-manager key-bindings key-state-atom]
  (let [keyinput-bindings (create-triggers key-bindings)
        hold-listener (reify ActionListener
                        (onAction [this name pressed tpf]
                          (swap! key-state-atom assoc (keyword name) pressed)))
        tap-listener (reify ActionListener
                       (onAction [this name pressed tpf]
                         (when pressed
                           (swap! key-state-atom
                                  update-in [:taps] conj (keyword name)))))
        filter-fn (fn [type]
                    (into-array
                      (map first (filter #(= type (% 2)) key-bindings))))
        tap-names (filter-fn :tap)
        hold-names (filter-fn :hold)]
    (doseq [[name trigger _] keyinput-bindings]
      (.addMapping input-manager name (into-array [trigger])))
    (.addListener input-manager hold-listener hold-names)
    (.addListener input-manager tap-listener tap-names)))
