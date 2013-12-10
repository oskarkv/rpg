(ns game.common.graphics
  (:import (com.jme3.math ColorRGBA Vector2f Vector3f Ray)
           com.jme3.asset.plugins.FileLocator
           (com.jme3.scene Geometry Mesh VertexBuffer VertexBuffer$Type Node)
           (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
           com.jme3.scene.shape.Box
           com.jme3.collision.CollisionResults
           com.jme3.util.BufferUtils
           com.jme3.material.Material
           com.jme3.renderer.queue.RenderQueue$Bucket
           com.jme3.font.BitmapText)
  (:require [game.common.core :as cc]
            [game.constants :as consts]))

(defn make-quad [x y]
  {:vertices (map #(Vector3f. (+ x %1) (+ y %2) 0)
                  [0 1 0 1] [0 0 1 1])
   :tex-coords (map #(Vector2f. %1 %2)
                    [0 1 0 1] [0 0 1 1])})

(defn portray-game-map [root-node asset-manager game-map]
  (let [w (count game-map)
        h (count (first game-map))
        quads (for [x (range w) y (range h)]
                (make-quad x y))
        vertices (into-array (mapcat :vertices quads))
        tex-coords (into-array (mapcat :tex-coords quads))
        indices (int-array (apply concat
                                  (for [x (range (* w h))]
                                    (map #(+ (* 4 x) %) [2 0 1 1 3 2]))))
        mesh (doto (Mesh.)
               (.setBuffer VertexBuffer$Type/Position 3
                           (BufferUtils/createFloatBuffer vertices))
               (.setBuffer VertexBuffer$Type/TexCoord 2
                           (BufferUtils/createFloatBuffer tex-coords))
               (.setBuffer VertexBuffer$Type/Index 3
                           (BufferUtils/createIntBuffer indices))
               (.updateBound))
        mat (doto (Material. asset-manager "Common/MatDefs/Misc/Unshaded.j3md")
              (.setTexture "ColorMap" (.loadTexture asset-manager
                                                    "test_ground.jpg")))
        geom (doto (Geometry. "ground" mesh) (.setMaterial mat))]
    (.attachChild root-node geom)))

(defn create-character-name-text [name height font]
  (let [name (or name "")
        text (doto (BitmapText. font false)
               (.setQueueBucket RenderQueue$Bucket/Transparent)
               (.setSize 0.5)
               (.setText name)
               (.addControl
                 (doto (BillboardControl.)
                   (.setAlignment BillboardControl$Alignment/Screen))))]
    (.setLocalTranslation text
                          (- (/ (.getLineWidth text) 2))
                          (/ (.getLineHeight text) 2)
                          height)
    text))

(defn create-character-geom [color asset-manager]
  (let [mat (doto (Material. asset-manager "Common/MatDefs/Misc/Unshaded.j3md")
              (.setColor "Color" color))
        r consts/player-radius
        box (Box. (Vector3f. 0 0 r) r r r)
        geom (doto (Geometry. "character-geom" box)
               (.setMaterial mat))]
    geom))

(defn new-group-node [name & children]
  (let [parent (Node. name)]
    (doseq [child children]
      (.attachChild parent child))
    parent))

(defn update-character [old-object character]
  (let [[x y] (:pos character)
        z (.getZ (.getLocalTranslation old-object))]
    (.setLocalTranslation old-object x y z)
    old-object))

(defn create-character-node [asset-manager character id]
  (let [geom (create-character-geom ColorRGBA/Blue asset-manager)
        font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
        name-text (create-character-name-text (:name character) 1 font)
        node (new-group-node (:name character) geom name-text)]
    {:node node :geom geom}))

(defn portray-character [old-object characters-node character id asset-manager]
  (if old-object
    {:node (update-character old-object character)}
    (let [{:keys [node geom]} (create-character-node asset-manager character id)]
      (.attachChild characters-node node)
      {:geom geom :node (update-character node character)})))

(defn portray-characters
  [ids->objects geoms->ids characters-node game-state asset-manager]
  (let [id-node-geom-list
        (for [[id character] (:chars game-state)]
          [id (portray-character (@ids->objects id) characters-node
                                 character id asset-manager)])
        get-id-node (fn [[id map]] (if-let [node (:node map)] [id node]))
        get-geom-id (fn [[id map]] (if-let [geom (:geom map)] [geom id]))
        ids->objects-addition
        (into {} (map get-id-node id-node-geom-list))
        geoms->ids-addition
        (into {} (map get-geom-id id-node-geom-list))]
    (swap! ids->objects merge ids->objects-addition)
    (swap! geoms->ids merge geoms->ids-addition)))

(defn pick-target* [input-manager characters-node camera geoms->ids]
  (let [mouse-coords (.getCursorPosition input-manager)
        [near far] (map #(.getWorldCoordinates camera mouse-coords %) [0 1])
        ray (Ray. near (doto far (.subtractLocal near) (.normalizeLocal)))
        results (CollisionResults.)
        _ (.collideWith characters-node ray results)
        picked (loop [results results ret nil dist 1e9]
                 (if-let [hit (first results)]
                   (let [id (geoms->ids (.getGeometry hit))
                         d (.getDistance hit)]
                     (println "PICKED:" id d)
                     (if (and id (< d dist))
                       (recur (next results) id d)
                       (recur (next results) ret dist)))
                   ret))]
    picked))

(deftype GraphicsSystem
  [root-node asset-manager ids->objects geoms->ids characters-node game-map]
  cc/Lifecycle
  (start [this]
    (.registerLocator asset-manager "assets" FileLocator)
    (portray-game-map root-node asset-manager game-map)
    (.attachChild root-node characters-node))
  (stop [this])
  cc/Updatable
  (update [this game-state]
    (portray-characters ids->objects geoms->ids
                        characters-node game-state asset-manager)))

(defn init-graphics-system [app game-map]
  (let [characters-node (Node. "characters-node")
        input-manager (.getInputManager app)
        asset-manager (.getAssetManager app)
        camera (.getCamera app)
        root-node (.getRootNode app)
        geoms->ids (atom {})]
    (defn pick-target []
      (pick-target* input-manager characters-node camera @geoms->ids))
    (->GraphicsSystem
      root-node asset-manager (atom {}) geoms->ids characters-node game-map)))
