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
            [game.constants :as consts])
  (:use game.utils))

(defn make-quad [x y]
  {:vertices (map #(Vector3f. (+ x %1) (+ y %2) 0)
                  [0 1 0 1] [0 0 1 1])
   :tex-coords (map #(Vector2f. %1 %2)
                    [0 1 0 1] [0 0 1 1])})

(defn portray-game-map [assets gamemap-node game-map]
  (println game-map)
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
        mat (get-in assets [:materials :ground])
        geom (doto (Geometry. "ground" mesh) (.setMaterial mat))]
    (.attachChild gamemap-node geom)))

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

(defn create-geom [assets type]
  (doto (Geometry. "character-geom" (get-in assets [:meshes type]))
    (.setMaterial (get-in assets [:materials type]))))

(defn new-group-node [name & children]
  (let [parent (Node. name)]
    (doseq [child children]
      (.attachChild parent child))
    parent))

(defn update-object [old-object object]
  (let [[x y] (:pos object)
        z (.getZ (.getLocalTranslation old-object))]
    (.setLocalTranslation old-object x y z)
    old-object))

(defn create-character-node [assets character]
  (let [geom (create-geom assets :player)
        name-text (create-character-name-text
                    (:name character) 1 (:font assets))
        node (new-group-node (:name character) geom name-text)]
    {:node node :geom geom}))

(defn create-spawn-node [assets spawn]
  (let [geom (create-geom assets :spawn)]
    {:geom geom :node geom}))

(defn portray-object
  [spatial type-node game-object assets create-fn update-fn]
  (if spatial
    {:node (update-fn spatial game-object)}
    (let [{:keys [node geom]} (create-fn assets game-object)]
      (.attachChild type-node node)
      {:geom geom :node (update-fn node game-object)})))

(defn portray-objects
  [objects ids->objects type-node assets create-fn update-fn]
  (for [[id object] objects]
    [id (portray-object (@ids->objects id) type-node object
                        assets create-fn update-fn)]))

(defn update-object-maps [ids->objects geoms->ids ids-and-objects]
  (let [get-id-node (fn [[id map]] (if-let [node (:node map)] [id node]))
        get-geom-id (fn [[id map]] (if-let [geom (:geom map)] [geom id]))
        ids->objects-addition
        (into {} (map get-id-node ids-and-objects))
        geoms->ids-addition
        (into {} (map get-geom-id ids-and-objects))]
    (swap! ids->objects merge ids->objects-addition)
    (swap! geoms->ids merge geoms->ids-addition)))

(defn get-collisions [objects collidables]
  (let [results (CollisionResults.)
        _ (.collideWith objects collidables results)]
    results))

(defn get-target-ray* [input-manager camera]
  (let [mouse-coords (.getCursorPosition input-manager)
        [near far] (map #(.getWorldCoordinates camera mouse-coords %) [0 1])
        ray (Ray. near (doto far (.subtractLocal near) (.normalizeLocal)))]
    ray))

(defn get-target-coords* [input-manager camera gamemap-node]
  (let [ray (get-target-ray* input-manager camera)
        collisions (get-collisions gamemap-node ray)
        collision (.getClosestCollision collisions)
        collision-point (if collision (.getContactPoint collision))
        pos (if collision-point
              [(.getX collision-point) (.getY collision-point)])]
    pos))

(defn pick-target** [input-manager node camera geoms->ids]
  (let [ray (get-target-ray* input-manager camera)
        results (get-collisions node ray)
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

(defn pick-target* [input-manager nodes camera geoms->ids type]
  (let [pick-target
        (fn [node]
          (when-let [id (pick-target** input-manager (node nodes)
                                       camera geoms->ids)]
            {:type node :id id}))]
    (if (seq? (type nodes))
      (some pick-target (type nodes))
      (:id (pick-target type)))))

(defn create-mother-material [asset-manager]
  (Material. asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))

(defn create-material [material textures texture color]
  (let [mat (.clone material)]
    (if texture (.setTexture mat "ColorMap" (textures texture)))
    (if color (.setColor mat "Color" color))
    mat))

(defn create-material-map [mother-material textures]
  (letfn [(color-material [color]
            (create-material mother-material textures nil color))]
    {:player (color-material ColorRGBA/Blue)
     :mob (color-material ColorRGBA/Red)
     :ground (create-material mother-material textures :ground nil)
     :spawn (color-material ColorRGBA/Red)}))

(defn create-texture-map [asset-manager]
  (letfn [(load-texture [tex]
            (.loadTexture asset-manager tex))]
    {:ground (load-texture "test_ground.jpg")}))

(defn create-mesh-map []
  {:player (Box. (Vector3f. 0 0 0.3) 0.3 0.3 0.3)
   :mob (Box. (Vector3f. 0 0 0.3) 0.3 0.3 0.3)
   :spawn (Box. (Vector3f. 0 0 0.1) 0.3 0.3 0.1)})

(deftype GraphicsSystem
  [nodes ids->objects geoms->ids game-map assets]
  cc/Lifecycle
  (start [this]
    (portray-game-map assets (:gamemap-node nodes) game-map)
    (fmap #(.attachChild (:root-node nodes) %) (dissoc nodes :root-node)))
  (stop [this])
  cc/Updatable
  (update [this game-state]
    ;;; Not all object types exist in both editor and client.
    (let [create-fns {:chars create-character-node
                      :spawns create-spawn-node}
          portray (fn [objects type node]
                    (portray-objects objects ids->objects node assets
                                     (create-fns type) update-object))
          chars (portray (:chars game-state) :chars (:characters-node nodes))
          objects (concat (portray (get-in game-state [:game-map :spawns])
                                   :spawns (:spawns-node nodes))
                          chars)]
      (update-object-maps ids->objects geoms->ids objects))))

(defn init-graphics-system [app game-map]
  (let [root-node (.getRootNode app)
        characters-node (Node. "characters-node")
        gamemap-node (Node. "gamemap-node")
        spawns-node (Node. "spawns-node")
        gamestate-to-node {:chars :characters-node
                           :spawns :spawns-node}
        nodes {:characters-node characters-node
               :gamemap-node gamemap-node
               :spawns-node spawns-node
               :root-node root-node}
        input-manager (.getInputManager app)
        asset-manager (doto (.getAssetManager app)
                        (.registerLocator "assets" FileLocator))
        camera (.getCamera app)
        geoms->ids (atom {})
        meshes (create-mesh-map)
        mother-material (create-mother-material asset-manager)
        textures (create-texture-map asset-manager)
        materials (create-material-map mother-material textures)
        font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
        assets {:meshes meshes :mother-material mother-material
                :textures textures :materials materials :font font}]
    (defn pick-target [type]
      (pick-target* input-manager nodes camera
                    @geoms->ids (type gamestate-to-node)))
    (defn get-target-ray []
      (get-target-ray* input-manager camera))
    (defn get-target-coords []
      (get-target-coords* input-manager camera (:gamemap-node nodes)))
    (->GraphicsSystem
      nodes (atom {}) geoms->ids game-map assets)))
