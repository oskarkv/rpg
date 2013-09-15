(ns game.common.graphics
  (:import (com.jme3.math ColorRGBA Vector2f Vector3f)
           com.jme3.asset.plugins.FileLocator
           (com.jme3.scene Geometry Mesh VertexBuffer VertexBuffer$Type Node)
           (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
           com.jme3.scene.shape.Box
           com.jme3.util.BufferUtils
           com.jme3.material.Material
           com.jme3.renderer.queue.RenderQueue$Bucket
           com.jme3.font.BitmapText)
  (:require [game.common.core :as cc]))

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
        box (Box. (Vector3f. 0 0 0.3) 0.3 0.3 0.3)
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

(defn portray-character [old-object characters-node character asset-manager]
  (if old-object
    (update-character old-object character)
    (let [model (create-character-geom ColorRGBA/Blue asset-manager)
          font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
          name-text (create-character-name-text (:name character) 1 font)
          node (new-group-node (:name character) model name-text)]
      (.attachChild characters-node node)
      (update-character node character))))

(defn portray-characters [ids->objects characters-node game-state asset-manager]
  (if-let [players (:players game-state)]
    (apply conj ids->objects
           (for [[id character] players]
             [id (portray-character (ids->objects id) characters-node
                                    character asset-manager)]))
    ids->objects))

(deftype GraphicsSystem
  [root-node asset-manager ids->objects characters-node game-map]
  cc/Lifecycle
  (start [this]
    (.registerLocator asset-manager "assets" FileLocator)
    (portray-game-map root-node asset-manager game-map)
    (.attachChild root-node characters-node))
  (stop [this])
  cc/Updatable
  (update [this game-state]
    (reset! ids->objects (portray-characters @ids->objects characters-node
                                             game-state asset-manager))))

(defn init-graphics-system [root-node asset-manager game-map]
  (->GraphicsSystem
    root-node asset-manager (atom {}) (Node. "characters-node") game-map))
