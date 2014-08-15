(ns game.common.graphics
  (:import (com.jme3.math ColorRGBA Vector2f Vector3f Ray)
           com.jme3.asset.plugins.FileLocator
           (com.jme3.scene Geometry Mesh VertexBuffer VertexBuffer$Type Node)
           (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
           com.jme3.scene.shape.Box
           com.jme3.input.ChaseCamera
           com.jme3.collision.CollisionResults
           com.jme3.util.BufferUtils
           com.jme3.material.Material
           com.jme3.renderer.queue.RenderQueue$Bucket
           (com.jme3.font BitmapText BitmapFont$Align Rectangle))
  (:require [game.common.core :as cc]
            [game.constants :as consts]
            [clojure.set :as set])
  (:use game.utils))

(defn get-collisions [objects collidable]
  (let [results (CollisionResults.)]
    (.collideWith objects collidable results)
    results))

(defn get-target-ray* [input-manager camera]
  (let [mouse-coords (.getCursorPosition input-manager)
        [near far] (map #(.getWorldCoordinates camera mouse-coords %) [0 1])]
    (Ray. near (doto far (.subtractLocal near) (.normalizeLocal)))))

(defn get-target-coords* [input-manager camera gamemap-node]
  (let [ray (get-target-ray* input-manager camera)]
    (some-> (get-collisions gamemap-node ray)
            .getClosestCollision
            .getContactPoint
            (juxt #(.getX %) #(.getY %)))))

(defn get-closest-model-collision [results geoms->ids]
  (let [iter (.iterator results)]
    (loop []
      (if (.hasNext iter)
        (if-let [id (some-> iter .next .getGeometry geoms->ids)]
          id
          (recur))))))

(defn pick-target* [input-manager node camera geoms->ids]
  (let [ray (get-target-ray* input-manager camera)
        results (get-collisions node ray)]
    (some-> results (get-closest-model-collision geoms->ids))))

(defn make-quad [x y]
  {:vertices (map #(Vector3f. (+ x %1) 0 (- (+ y %2)))
                  [0 1 0 1] [0 0 1 1])
   :tex-coords (map #(Vector2f. %1 %2)
                    [0 1 0 1] [0 0 1 1])})

(defn portray-game-map [assets gamemap-node game-map]
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
        text (doto (BitmapText. font)
               (.setQueueBucket RenderQueue$Bucket/Transparent)
               (.setSize 0.5)
               (.setText name))
        w (.getLineWidth text)
        h (.getLineHeight text)]
    (doto text
      (.setBox (Rectangle. (- w) (/ h 2) (* w 2) h))
      (.setAlignment BitmapFont$Align/Center)
      (.addControl
        (doto (BillboardControl.)
          (.setAlignment BillboardControl$Alignment/Screen)))
      (.setLocalTranslation 0 height 0))))

(defn create-geom [assets type]
  (doto (Geometry. (str type "-geom") (get-in assets [:meshes type]))
    (.setMaterial (get-in assets [:materials type]))))

(defn new-group-node [name & children]
  (let [parent (Node. name)]
    (doseq [child children]
      (.attachChild parent child))
    parent))

(defn create-character-node [assets character]
  (let [geom (create-geom assets (:type character))
        name-text (create-character-name-text
                    (:name character) 1 (:font assets))
        node (new-group-node (:name character) geom name-text)]
    {:node node :geom geom}))

(defn create-spawn-node [assets spawn]
  (let [geom (create-geom assets :spawn)]
    {:geom geom :node geom}))

(defn update-object [old-object game-object]
  (let [[x y] (:pos game-object)
        z (.getY (.getLocalTranslation (:node old-object)))]
    (.setLocalTranslation (:node old-object) x z (- y))
    old-object))

(defn get-graphics-object [[id game-object] ids->objects create-fn update?]
  (let [update #(update-object % game-object)]
    [id (or (some-> (ids->objects id) (cond-> update? update))
            (update (create-fn game-object)))]))

(defn update-object-maps [ids->objects geoms->ids ids-and-objects]
  (let [get-geom-id (fn [[id obj]] [(:geom obj) id])]
    (reset! ids->objects (into {} ids-and-objects))
    (reset! geoms->ids (into {} (map get-geom-id ids-and-objects)))))

(deftype GraphicsSystem
  [nodes ids->objects geoms->ids game-map assets]
  cc/Lifecycle
  (start [this]
    (portray-game-map assets (:gamemap nodes) game-map)
    (fmap #(.attachChild (:root-node nodes) %) (dissoc nodes :root-node)))
  (stop [this])
  cc/EventsProducer
  (get-events [this])
  cc/Updatable
  (update [this game-state]
    ;;; Not all object types exist in both editor and client.
    (let [get-maker (fn [create-fn update?]
                      #(get-graphics-object % @ids->objects create-fn update?))
          create-char #(create-character-node assets %)
          create-spawn #(create-spawn-node assets %)
          get-char (get-maker create-char true)
          get-spawn (get-maker create-spawn true)
          get-corpse (get-maker create-char false)
          chars (map get-char (:chars game-state))
          spawns (map get-spawn (get-in game-state [:game-map :spawns]))
          corpses (map get-char (:corpses game-state))
          attach-objects (fn [id-objs node]
                           (doseq [[id obj] id-objs]
                             (.attachChild node (:node obj))))
          reattach (fn [k objs]
                     (.detachAllChildren (k nodes))
                     (attach-objects objs (k nodes)))]
      (dorun (map reattach [:chars :spawns :corpses] [chars spawns corpses]))
      (update-object-maps ids->objects geoms->ids
                          (concat chars spawns corpses)))))

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

(defn create-mesh-map [asset-manager]
  {:player (Box. (Vector3f. 0 0.3 0) 0.3 0.3 0.3)
   :mob (Box. (Vector3f. 0 0.3 0) 0.3 0.3 0.3)
   :spawn (Box. (Vector3f. 0 0.1 0) 0.3 0.3 0.1)})

(defn set-up-camera* [ids->objects camera input-manager game-state]
  (let [own-object (-> game-state :own-id ids->objects :node)]
    (doto (ChaseCamera. camera own-object input-manager)
      (.setInvertVerticalAxis true)
      (.setRotationSpeed consts/camera-rotation-speed))))

(defn get-camera-dir* [camera]
  (let [dir (-> camera .getDirection (.setY 0))
        to-game-dir (juxt #(.x %) #(- (.z %)))]
    (if (= 0 (.x dir) (.y dir))
      (-> camera .getUp to-game-dir)
      (-> dir .normalizeLocal to-game-dir))))

(defn init-graphics-system [app game-map]
  (let [root-node (.getRootNode app)
        characters-node (Node. "characters-node")
        gamemap-node (Node. "gamemap-node")
        spawns-node (Node. "spawns-node")
        corpses-node (Node. "corpses-node")
        nodes {:chars characters-node
               :player characters-node
               :mob characters-node
               :spawns spawns-node
               :corpses corpses-node
               :gamemap gamemap-node
               :root-node root-node}
        input-manager (.getInputManager app)
        asset-manager (doto (.getAssetManager app)
                        (.registerLocator "assets" FileLocator))
        camera (.getCamera app)
        ids->objects (atom {})
        geoms->ids (atom {})
        meshes (create-mesh-map asset-manager)
        mother-material (create-mother-material asset-manager)
        textures (create-texture-map asset-manager)
        materials (create-material-map mother-material textures)
        font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
        assets {:meshes meshes :mother-material mother-material
                :textures textures :materials materials :font font}]
    (.lookAtDirection camera (Vector3f. 0 -1 0) (Vector3f. 0 0 1))
    (.setLocation camera (Vector3f. 0 10 0))
    (defn pick-target [type]
      (pick-target* input-manager (type nodes) camera @geoms->ids))
    (defn get-target-ray []
      (get-target-ray* input-manager camera))
    (defn get-target-coords []
      (get-target-coords* input-manager camera (:gamemap nodes)))
    (defn set-up-camera [graphics-system game-state]
      ;; We need to update once to make sure that the player exists in the
      ;; scene graph
      (cc/update graphics-system game-state)
      (set-up-camera* @ids->objects camera input-manager game-state))
    (defn get-camera-dir []
      (get-camera-dir* camera))
    (->GraphicsSystem nodes ids->objects geoms->ids game-map assets)))
