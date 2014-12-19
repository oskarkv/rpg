(ns game.common.graphics
  (:import (com.jme3.math ColorRGBA Vector2f Vector3f Ray)
           com.jme3.asset.plugins.FileLocator
           (com.jme3.scene Geometry Mesh VertexBuffer VertexBuffer$Type Node)
           (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
           com.jme3.scene.shape.Box
           com.jme3.input.ChaseCamera
           com.jme3.collision.CollisionResults
           com.jme3.util.BufferUtils
           (com.jme3.material Material RenderState$BlendMode)
           (com.jme3.effect.shapes EmitterShape EmitterPointShape)
           (com.jme3.effect ParticleEmitter ParticleMesh$Type)
           com.jme3.renderer.queue.RenderQueue$Bucket
           (com.jme3.font BitmapText BitmapFont$Align Rectangle))
  (:require (game.common [core :as cc]
                         [jme-utils :as ju])
            [game.constants :as consts]
            [clojure.set :as set])
  (:use game.utils))

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

(defmacro make-particle-emitter-fn [kw-dval-code-vecs]
  (with-gensyms [settings]
    `(fn [~settings]
       (doto (ParticleEmitter. "ParticleEmitter" ParticleMesh$Type/Triangle 0)
         ~@(for [[kw dv code] kw-dval-code-vecs]
             `(~code (or (~kw ~settings) ~dv)))))))

(defn emitter-shape [rand-point rand-normal]
  (let [set-vector (fn [v vals]
                     (doto v
                       (.setX (vals 0)) (.setY (vals 1)) (.setZ (vals 2))))]
    (reify EmitterShape
      (deepClone [this]
        (.clone this))
      (getRandomPoint [this store]
        (let [p (rand-point)]
          (set-vector store p)))
      (getRandomPointAndNormal [this point normal]
        (.getRandomPoint this point)
        (let [n (rand-normal)]
          (set-vector normal n))))))

(defn particle-emitter [materials settings]
  ((make-particle-emitter-fn
     [[:texture :flash #(.setMaterial %1 (%2 materials))]
      [:num-particles 50 .setNumParticles]
      [:images-x 2 .setImagesX]
      [:images-y 2 .setImagesY]
      ;; If select-random-image is false,
      ;; the image depends on the particles age.
      [:select-random-image true .setSelectRandomImage]
      ;; By default in-world-space and ignore-transform has the same value.
      ;; If in-world-space is true, the particles' positions are determined by
      ;; a custom calculation that takes scale into account, but the size of
      ;; the particles themselves does not scale.
      [:in-world-space false .setInWorldSpace]
      [:ignore-transform false .setIgnoreTransform]
      [:start-color ColorRGBA/Red .setStartColor]
      [:end-color ColorRGBA/Red .setEndColor]
      [:velocity-variation 0
       #(-> %1 .getParticleInfluencer (.setVelocityVariation %2))]
      [:initial-velocity [0 0 0]
       #(-> %1 .getParticleInfluencer (.setInitialVelocity (ju/vectorf %2)))]
      [:high-life 3 .setHighLife]
      [:low-life 2 .setLowLife]
      [:gravity [0 0 0] #(.setGravity %1 (.negate (ju/vectorf %2)))]
      [:start-size 0.5 .setStartSize]
      [:end-size 0.5 .setEndSize]
      [:pps 5 .setParticlesPerSec]
      [:rot-speed 0 .setRotateSpeed]
      [:shape (EmitterPointShape. (Vector3f. 0 0 0)) .setShape]
      [:random-angle true .setRandomAngle]])
   settings))

(def emitter-shapes
  (let [rand-normal (fn [] (vec (take 3 (repeatedly #(rand-uniform -1 1)))))]
    {:circle
     (emitter-shape
       (fn [] (let [v (rand-uniform (* 2 Math/PI))]
                [(Math/cos v) 0 (Math/sin v)]))
       rand-normal)}))

(def effects
  {:regrowth {:gravity [0 0.3 0]
              :start-color ColorRGBA/Green
              :end-color ColorRGBA/Yellow
              :start-size 0.15
              :end-size 0.1
              :velocity-variation 0.1
              :shape :circle}})

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
  (update [this {:keys [game-state events]}]
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

(defn create-base-materials [asset-manager]
  (let [mat #(Material. asset-manager %)]
    {:unshaded (mat "Common/MatDefs/Misc/Unshaded.j3md")
     :particle (mat "Common/MatDefs/Misc/Particle.j3md")}))

(defn create-material [material textures texture color]
  (let [mat (.clone material)]
    (if texture (.setTexture mat "ColorMap" (textures texture)))
    (if color (.setColor mat "Color" color))
    mat))

(defn create-material-map [base-materials textures]
  (letfn [(color-material [color]
            (create-material (:unshaded base-materials) textures nil color))
          (particle-material [texture]
            (doto (.clone (:particle base-materials))
              (.setTexture "Texture" (texture textures))
              (-> .getAdditionalRenderState (.setPointSprite true))))]
    (merge base-materials
      {:player (color-material ColorRGBA/Blue)
       :mob (color-material ColorRGBA/Red)
       :ground (create-material (:unshaded base-materials) textures :ground nil)
       :spawn (color-material ColorRGBA/Red)
       :flash (particle-material :flash)
       :shockwave (particle-material :shockwave)
       :flame (particle-material :flame)})))

(defn create-texture-map [asset-manager]
  (letfn [(load-texture [tex]
            (.loadTexture asset-manager tex))]
    {:ground (load-texture "test_ground.jpg")
     :flash (load-texture "textures/flash.png")
     :flame (load-texture "textures/flame.png")
     :shockwave (load-texture "textures/shockwave.png")}))

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
        base-materials (create-base-materials asset-manager)
        textures (create-texture-map asset-manager)
        materials (create-material-map base-materials textures)
        font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
        assets {:meshes meshes :textures textures
                :materials materials :font font}]
    (defn pick-target [type]
      (let [ray (ju/get-world-ray input-manager camera)]
        (ju/pick-target ray (type nodes) @geoms->ids)))
    (defn get-target-coords []
      (ju/get-target-coords input-manager camera (:gamemap nodes)))
    (defn set-up-camera [graphics-system game-state]
      ;; Update once to make sure that the player exists in the scene graph
      (cc/update graphics-system {:game-state game-state})
      (set-up-camera* @ids->objects camera input-manager game-state))
    (defn get-camera-dir []
      (get-camera-dir* camera))
    (->GraphicsSystem nodes ids->objects geoms->ids game-map assets)))
