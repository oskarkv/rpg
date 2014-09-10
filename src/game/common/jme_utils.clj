(ns game.common.jme-utils
  (:refer-clojure :exclude [vec])
  (:import (com.jme3.math Vector2f Vector3f Ray)
           (com.jme3.collision CollisionResults)))

(defprotocol Vecable
  (vec [this]))

(extend-protocol Vecable
  Vector2f
  (vec [this] (vector (.x this) (.y this)))
  Vector3f
  (vec [this] (vector (.x this) (.y this) (.z this))))

(defn get-collisions [objects collidable]
  (let [results (CollisionResults.)]
    (.collideWith objects collidable results)
    results))

(defn get-world-ray [input-manager camera]
  (let [mouse-coords (.getCursorPosition input-manager)
        [near far] (map #(.getWorldCoordinates camera mouse-coords %) [0 1])]
    (Ray. near (doto far (.subtractLocal near) (.normalizeLocal)))))

(defn get-screen-ray [[mx my]]
  (Ray. (Vector3f. mx my 1000) (Vector3f. 0 0 -1)))

(defn get-target-coords [input-manager camera gamemap-node]
  (let [ray (get-world-ray input-manager camera)]
    (some-> (get-collisions gamemap-node ray)
            .getClosestCollision
            .getContactPoint
            vec)))

(defn get-closest-model-collision [results geoms->ids]
  (first (remove nil? (map #(-> % .getGeometry geoms->ids) results))))

(defn pick-target [ray node geoms->things]
  (-> (get-collisions node ray)
      (get-closest-model-collision geoms->things)))