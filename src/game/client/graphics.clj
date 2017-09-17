(ns game.client.graphics
  (:require
   [game.client.jme-utils :as ju]
   [game.common.core :as cc]
   [game.constants :as consts]
   [game.game-map :as gmap]
   [game.math :as math]
   [game.utils :refer :all])
  (:import
   (com.jme3.asset.plugins FileLocator)
   (com.jme3.effect ParticleEmitter ParticleMesh$Type)
   (com.jme3.effect.shapes EmitterPointShape EmitterShape)
   (com.jme3.font BitmapFont$Align BitmapText Rectangle)
   (com.jme3.input ChaseCamera)
   (com.jme3.material Material)
   (com.jme3.math ColorRGBA Vector2f Vector3f)
   (com.jme3.renderer.queue RenderQueue$Bucket)
   (com.jme3.scene Geometry Mesh Node VertexBuffer VertexBuffer$Type)
   (com.jme3.scene.control BillboardControl BillboardControl$Alignment)
   (com.jme3.scene.shape Box)
   (com.jme3.util BufferUtils)))

(defn make-quad [x y]
  {:vertices (map #(Vector3f. (+ x %1) 0 (- (+ y %2)))
                  [0 1 0 1] [0 0 1 1])
   :tex-coords (map #(Vector2f. %1 %2)
                    [0 1 0 1] [0 0 1 1])})

(defn portray-game-map [assets game-map-node game-map]
  (let [w (count game-map)
        h (count (first game-map))
        quads (for [x (range w) y (range h)
                    :when (gmap/traversable? (get-in game-map [x y]))]
                (make-quad x y))
        vertices (into-array (mapcat :vertices quads))
        tex-coords (into-array (mapcat :tex-coords quads))
        indices (int-array (apply concat
                                  (for [x (range (count quads))]
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
    (.attachChild game-map-node geom)))

(defn create-character-name-text [name height font]
  (let [name (or name "")
        on-material (fn [text f]
                      (runmap #(-> % .getMaterial .getAdditionalRenderState f)
                              (.getChildren text)))
        text (doto (BitmapText. font)
               (.setQueueBucket RenderQueue$Bucket/Transparent)
               (on-material #(.setAlphaTest % true))
               (on-material #(.setAlphaFallOff % 0.3))
               (.setSize 0.3)
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
    (make-map node geom)))

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

(defn update-object-maps [gfx-state ids-and-objects]
  (let [get-geom-id (fn [[id obj]] [(:geom obj) id])]
    (-> gfx-state
      (assoc :ids->objects (into {} ids-and-objects))
      (assoc :geoms->ids (into {} (map get-geom-id ids-and-objects))))))

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
     [:num-particles 100 .setNumParticles]
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
      (fn [] (let [v (rand-uniform math/tau)]
               [(* 0.5 (math/cos v)) 0 (* 0.5 (math/sin v))]))
      rand-normal)
     :time-circle
     (emitter-shape
      (fn [] (let [period 500
                   v (* (/ (mod (current-time-ms) period) period)
                        math/tau (rand-uniform 0.9 1.1))]
               [(* 0.5 (math/cos v)) 0 (* 0.5 (math/sin v))]))
      rand-normal)}))

(def effects
  (->>
    {:regrowth {:duration 2
                :position [0 0.7 0]
                :gravity [0 -0.4 0]
                :start-color ColorRGBA/Blue
                :end-color ColorRGBA/Green
                :start-size 0.25
                :end-size 0.1
                :pps 30
                :shape :time-circle}
     :poison {:start-color (ColorRGBA. 0 0.6 0 1)
              :end-color (ColorRGBA. 0 0.6 0 1)
              :start-size 0
              :end-size 0.4
              :rot-speed 1
              :high-life 2
              :low-life 1
              :initial-velocity [0 0.7 0]
              :gravity [0 -0.3 0]
              :velocity-variation 0.5
              :pps 10
              :texture :flame
              :images-x 1
              :images-y 1}}
    (fmap (fn [m] (merge {:duration 1 :position [0 0 0]} m)))
    (fmap (fn [m] (update m :shape emitter-shapes)))))

(defmulti process-event (fn [gfx-state game-state event] (:type event)))

(defmethod process-event :default [gfx-state game-state event])

(defmethod process-event :s-spell-cast [gfx-state game-state event]
  (let [{:keys [by target spell]} event
        mats (get-in gfx-state [:assets :materials])
        settings (spell effects)
        emitter (particle-emitter mats settings)]
    (.setLocalTranslation emitter (ju/vectorf (:position settings)))
    (.attachChild (get-in gfx-state [:ids->objects target :node]) emitter)
    (update gfx-state :effects conj
            {:node emitter :end-time (+ (current-time-ms)
                                        (* 1000 (:duration settings)))})))

(defn process-events [gfx-state game-state events]
  (reduce (fn [gfx e] (or (process-event gfx game-state e) gfx))
          gfx-state events))

(defn expired-and-remains [coll delay-ms]
  (let [curr-time (current-time-ms)
        piles (group-by #(< curr-time (+ delay-ms (:end-time %))) coll)]
    [(piles false) (piles true)]))

(defn remove-dead-effects [gfx-state]
  (let [[expired remains] (expired-and-remains (:dying-effects gfx-state) 5000)]
    (runmap #(.removeFromParent (:node %)) expired)
    (assoc gfx-state :dying-effects remains)))

(defn update-effects [gfx-state]
  (let [[expired remains] (expired-and-remains (:effects gfx-state) 0)]
    (runmap #(.setParticlesPerSec (:node %) 0) expired)
    (-> gfx-state
      (assoc :effects remains)
      (update :dying-effects into expired)
      remove-dead-effects)))

(deftype GraphicsSystem [gfx-state-atom]
  cc/Lifecycle
  (start [this]
    (let [gfx-state @gfx-state-atom
          {:keys [nodes assets game-map]} gfx-state]
      (portray-game-map assets (:game-map nodes) game-map)
      (fmap #(.attachChild (:root-node nodes) %) (dissoc nodes :root-node))))
  (stop [this])
  cc/EventsProducer
  (get-events [this])
  cc/Updatable
  (update [this {:keys [game-state events]}]
    ;;; Not all object types exist in both editor and client.
    (let [{:keys [nodes assets ids->objects geoms->ids]} @gfx-state-atom
          get-maker (fn [create-fn update?]
                      #(get-graphics-object % ids->objects create-fn update?))
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
      ;; Removes obsolete objects
      (runmap reattach [:chars :spawns :corpses] [chars spawns corpses])
      (swap! gfx-state-atom
             #(-> %
                (update-object-maps (concat chars spawns corpses))
                (process-events game-state events)
                update-effects)))))

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
            :ground (create-material (:unshaded base-materials)
                                     textures :ground nil)
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
        game-map-node (Node. "game-map-node")
        spawns-node (Node. "spawns-node")
        corpses-node (Node. "corpses-node")
        nodes {:chars characters-node
               :player characters-node
               :mob characters-node
               :spawns spawns-node
               :corpses corpses-node
               :game-map game-map-node
               :root-node root-node}
        input-manager (.getInputManager app)
        asset-manager (doto (.getAssetManager app)
                        (.registerLocator "assets" FileLocator))
        camera (.getCamera app)
        meshes (create-mesh-map asset-manager)
        base-materials (create-base-materials asset-manager)
        textures (create-texture-map asset-manager)
        materials (create-material-map base-materials textures)
        font (.loadFont asset-manager "Interface/Fonts/Default.fnt")
        assets (make-map meshes textures materials font)
        gfx-state-atom (atom {:nodes nodes :ids->objects {} :geoms->ids {}
                              :game-map game-map :assets assets :effects []})]
    (defn pick-target [type]
      (let [ray (ju/get-world-ray input-manager camera)]
        (ju/pick-target ray (type nodes) (:geoms->ids @gfx-state-atom))))
    (defn get-target-coords []
      (ju/get-target-coords input-manager camera (:game-map nodes)))
    (defn set-up-camera [graphics-system game-state]
      ;; Update once to make sure that the player exists in the scene graph
      (cc/update graphics-system {:game-state game-state})
      (set-up-camera* (:ids->objects @gfx-state-atom) camera
                      input-manager game-state))
    (defn get-camera-dir []
      (get-camera-dir* camera))
    (->GraphicsSystem gfx-state-atom)))
