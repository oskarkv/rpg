(ns game.client.input
  (:import (com.jme3.input KeyInput
                           MouseInput)
           (com.jme3.input.controls KeyTrigger
                                    ActionListener)))

(defn load-key-bindings [] [["forward" "w" :hold]
                            ["back" "r" :hold]
                            ["left" "a" :hold]
                            ["right" "s" :hold]])

(defn string->KeyInput [s]
  (-> (.getField KeyInput (str "KEY_" (.toUpperCase s))) (.get nil)))

(defn create-KeyInputs [key-bindings]
  (map (fn [[name key type]] [name (string->KeyInput key) type])
       key-bindings))

(defn create-key-state-map [key-bindings]
  (zipmap (map (comp keyword first)
               (filter (fn [[_ _ type]] (= type :hold)) key-bindings))
          (repeat false)))

(defn start-input [input-manager key-bindings key-state-atom]
  (let [keyinput-bindings (create-KeyInputs key-bindings)
        hold-listener (reify ActionListener
                        (onAction [this name pressed tpf]
                          (swap! key-state-atom assoc (keyword name) pressed)))]
    (doseq [[name key _] keyinput-bindings]
      (.addMapping input-manager name (into-array [(KeyTrigger. key)])))
    (.addListener input-manager
                  hold-listener
                  (into-array (map first key-bindings)))))

