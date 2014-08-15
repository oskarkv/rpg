(ns game.client.hudlib
  (:use game.utils)
  (:require [game.constants :as consts]
            [clojure.math.numeric-tower :as math]
            [game.common.core :as cc]
            [game.common.core-functions :as ccfns]
            [game.common.jme-utils :as ju])
  (:import (com.jme3.math Vector3f ColorRGBA)
           (com.jme3.font BitmapText BitmapFont$VAlign BitmapFont$Align
                          LineWrapMode Rectangle)
           (com.jme3.renderer.queue RenderQueue$Bucket)
           (com.jme3.material Material RenderState$BlendMode)
           (com.jme3.input RawInputListener)
           (com.jme3.scene Node Geometry)
           (com.jme3.scene.shape Quad)))

(defprotocol HudNode
  (add-child [this child])
  (remove-child [this child])
  (get-children [this])
  (remove-all-children [this]))

(defprotocol Sized
  (set-size [this size])
  (get-size [this])
  (auto-size [this] [this margin]))

(defprotocol Positioned
  (set-position [this pos])
  (set-depth [this z])
  (get-position [this]))

(defprotocol Textual
  (set-text [this string])
  (set-font-size [this size])
  (set-alignment [this align]))

(defprotocol Pictorial
  (set-material [this material])
  (set-color [this color]))

(defprotocol Described
  (set-tooltip [this string])
  (get-tooltip [this]))

(defprotocol Titled
  (set-title [this title])
  (set-header-color [this color])
  (set-header-material [this material]))

(defn hudnode-code [node-name children-name]
  `(HudNode
     (add-child
       [this# child#]
       (.attachChild ~node-name (:node child#))
       (swap! ~children-name conj child#)
       this#)
     (remove-child
       [this# child#]
       (.detachChild ~node-name (:node child#))
       (swap! ~children-name disj child#)
       this#)
     (get-children [this#] @~children-name)
     (remove-all-children
       [this#]
       (dorun (map (fn [child#] (remove-child this# child#)) @~children-name))
       this#)))

(defn positioned-code [node-name]
  `(Positioned
     (set-position
       [this# [x# y#]]
       (.setLocalTranslation
         ~node-name
         (Vector3f. x# (- y#) (.z (.getLocalTranslation ~node-name))))
       this#)
     (set-depth
       [this# z#]
       (set! (.z (.getLocalTranslation ~node-name)) z#)
       this#)
     (get-position
       [this#]
       (let [v# (.getLocalTranslation ~node-name)]
         [(.x v#) (- (.y v#))]))))

(defn forward-code [protocol target]
  `(~protocol
     ~@(for [{:keys [arglists name]} (vals (:sigs @(ns-resolve *ns* protocol)))
             :let [args (first arglists)]]
         (list name args (list* name target (vec (drop 1 args)))))))

(defn get-width [element]
  ((get-size element) 0))

(defn get-height [element]
  ((get-size element) 1))

(defn set-width [element w]
  (set-size element [w (get-height element)]))

(defn set-height [element h]
  (set-size element [(get-width element) h]))

(defmacro defhudrecord [name args & body]
  `(defrecord ~name ~args
     ~@(hudnode-code 'node 'children)
     ~@body))

(defmacro defposrecord [name args & body]
  `(defhudrecord ~name ~args
     ~@(positioned-code (first args))
     ~@body))

(defhudrecord Screen [material font node input-manager asset-manager
                      geoms->elements event-queue children start-fn]
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue event-queue))
  cc/Lifecycle
  (start [this]
    (when start-fn (start-fn)))
  (stop [this]))

(defposrecord ContainerElement [node children])

(defposrecord PictureElement [node geom tooltip-atom children]
  Sized
  (set-size [this [w h]]
    ;; We want the Quad to extend downwards
    (-> geom .getMesh (.updateGeometry w (- h) true))
    (.updateModelBound geom)
    this)
  (get-size [this]
    (-> geom .getMesh ((juxt #(.getWidth %) #(- (.getHeight %))))))
  (auto-size [this margin]
    (if (seq @children)
      (let [edges (map (fn [e] (map + (get-position e) (get-size e))) @children)
            xs (map first edges)
            ys (map second edges)
            calc #(+ (apply max %) margin)]
        (set-size this (mapv calc [xs ys])))
      this))
  (auto-size [this]
    (auto-size this 0))
  Described
  (set-tooltip [this string]
    (reset! tooltip-atom string)
    this)
  (get-tooltip [this]
    @tooltip-atom)
  Pictorial
  (set-material [this material]
    (.setMaterial geom material)
    this)
  (set-color [this [r g b a]]
    (-> geom .getMaterial (.setColor "Color" (ColorRGBA. r g b a)))))

(eval
  `(defrecord ~'WindowElement ~'[node body header title]
     ~@(positioned-code 'node)
     ~@(forward-code 'HudNode 'body)
     ~@(forward-code 'Pictorial 'body)
     ~@(forward-code 'Described 'body)
     Sized
     ~'(set-size [this [w h :as size]]
         (set-size body size)
         (set-size header [w consts/header-height])
         (set-size title [w consts/header-height]))
     ~'(get-size [this] (get-size body))
     ~'(auto-size [this] (auto-size this 0))
     ~'(auto-size [this margin]
         (auto-size body margin)
         (let [w (get-width body)]
           (set-width header w)
           (set-width title w)))
     Titled
     ~'(set-title [this text]
         (set-text title text))
     ~'(set-header-color [this color]
         (set-color header color))
     ~'(set-header-material [this material]
         (set-material header material))))

(def alignments {:center BitmapFont$Align/Center
                 :left BitmapFont$Align/Left
                 :right BitmapFont$Align/Right})

(defposrecord TextElement [node geom children]
  Sized
  (set-size [this [w h]]
    ;; The box extends downwards.
    (.setBox node (Rectangle. 0 0 w h))
    this)
  (get-size [this]
    (-> node ((juxt #(.getLineWidth %) #(.getHeight %)))))
  (auto-size [this]
    (.setBox node nil)
    (let [w (.getLineWidth node)
          h (.getHeight node)]
      (set-size this [w h])))
  Textual
  (set-text [this string]
    (.setText node string)
    this)
  (set-font-size [this size]
    (.setSize node size)
    this)
  (set-alignment [this align]
    (.setAlignment node (align alignments))))

(defn new-node []
  (Node. (str (ccfns/get-new-id))))

(defn new-geom [mesh]
  (Geometry. (str (ccfns/get-new-id)) mesh))

(defn get-material [screen texture-name]
  (let [{:keys [material asset-manager]} screen]
    (doto (.clone material)
      (.setTexture "ColorMap" (.loadTexture asset-manager texture-name)))))

(def get-material (memoize get-material))

(defn add-general-functionality
  [{:keys [geoms->elements]} element {:keys [clickable tooltip]}]
  (when clickable (swap! geoms->elements assoc (:geom element) element))
  (some->> tooltip (set-tooltip element)))

(defn set-element-defaults [element pos size]
  (doto element (set-position pos) (set-size size) (set-depth 0.1)))

(defn create-element
  ([screen pos size] (create-element screen pos size nil))
  ([screen pos size {:keys [texture-name clickable tooltip] :as options}]
   (let [{:keys [material geoms->elements]} screen
         mesh (Quad. 0 0 true)
         geom (new-geom mesh)
         node (new-node)
         element (->PictureElement node geom (atom nil) (atom #{}))]
     (set-material element
                   (if texture-name
                     (get-material screen texture-name)
                     (.clone material)))
     (.attachChild node geom)
     (set-element-defaults element pos size)
     (add-general-functionality screen element options)
     element)))

(defn create-text-element
  ([screen pos size] (create-text-element screen pos size nil))
  ([screen pos size options]
   (let [{:keys [font geoms->elements]} screen
         node (BitmapText. font)
         geom (first (.getChildren node))
         element (->TextElement node geom (atom #{}))]
     (set-element-defaults element pos size)
     (add-general-functionality screen element options)
     element)))

(defn create-window [screen]
  (let [hh consts/header-height
        node (new-node)
        body (create-element screen [0 hh] [0 0])
        header (create-element screen [0 0] [0 hh])
        title (create-text-element screen [0 0] [0 hh])]
    (.setLineWrapMode (:node title) LineWrapMode/NoWrap)
    (.attachChild node (:node body))
    (.attachChild node (:node header))
    (.attachChild node (:node title))
    (set-alignment title :center)
    (->WindowElement node body header title)))

(defn create-container [pos]
  (doto (->ContainerElement (new-node) (atom #{})) (set-position pos)))

(defn input-listener [callback]
  (let [listener (proxy [RawInputListener] [])
        not-used-one-arg '[beginInput endInput]
        not-used '[onJoyAxisEvent onJoyButtonEvent
                   onKeyEvent onMouseMotionEvent onTouchEvent]
        used '[onMouseButtonEvent]
        make-map (fn [lst f] (zipmap (map str lst) (repeat f)))]
    (update-proxy listener
                  (merge (make-map used (fn [this e] (callback e)))
                         (make-map not-used (fn [this e]))
                         (make-map not-used-one-arg (fn [this]))))))

(defn get-real-mouse-pos [screen]
  (-> screen :input-manager .getCursorPosition ju/vec))

(defn get-mouse-pos [screen]
  (update-in (get-real-mouse-pos screen) [1] #(- consts/resolution-y %)))

(defn get-element-under-cursor [{:keys [geoms->elements node] :as screen}]
  (let [ray (ju/get-screen-ray (get-real-mouse-pos screen))]
    (ju/pick-target ray node @geoms->elements)))

(defn make-click-callback [screen]
  (fn [event]
    (when-let [elem (get-element-under-cursor screen)]
      (.setConsumed event)
      (ccfns/queue-conj (:event-queue screen)
                        {:element elem
                         :button (.getButtonIndex event)
                         :pressed (.isPressed event)}))))

(defn create-screen [app]
  (let [asset-manager (.getAssetManager app)
        input-manager (.getInputManager app)
        event-queue (ref [])
        node (new-node)
        start-fn (fn []
                   (.setLocalTranslation
                     node (Vector3f. 0 consts/resolution-y 0))
                   (.setQueueBucket node RenderQueue$Bucket/Gui)
                   (.addRawInputListener
                     input-manager
                     (input-listener (make-click-callback screen)))
                   (.attachChild (.getGuiNode app) node))]
    (Screen.
      (doto (Material. asset-manager
                       "Common/MatDefs/Misc/Unshaded.j3md")
        (.. (getAdditionalRenderState)
            (setBlendMode RenderState$BlendMode/Alpha)))
      (.loadFont asset-manager "Interface/Fonts/Default.fnt")
      node
      input-manager
      asset-manager
      (atom {})
      event-queue
      (atom #{})
      start-fn)))
