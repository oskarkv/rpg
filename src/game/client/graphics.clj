(ns game.client.graphics
  (:import (com.jme3.math ColorRGBA Vector2f Vector3f)
           com.jme3.asset.plugins.FileLocator
           (com.jme3.scene Geometry Mesh VertexBuffer VertexBuffer$Type Node)
           com.jme3.util.BufferUtils
           com.jme3.material.Material
           (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
           com.jme3.renderer.queue.RenderQueue$Bucket
           com.jme3.font.BitmapText))

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
