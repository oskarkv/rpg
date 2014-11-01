(ns game.client.hudlib
  (:use game.utils)
  (:require [game.constants :as consts]
            [clojure.math.numeric-tower :as math]
            [clojure.walk :as walk]
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
  (remove-all-children [this])
  (get-parent [this])
  (set-parent [this parent])
  (is-focusable [this]))

(defprotocol Sized
  (set-size [this size])
  (get-size [this])
  (auto-size [this] [this margin]))

(defprotocol Positioned
  (set-position [this pos])
  (set-depth [this z])
  (get-position [this]))

(defprotocol Textual
  (set-text [this text])
  (set-font-size [this size])
  (set-alignment [this align]))

(defprotocol Pictorial
  (set-material [this material])
  (set-color [this color]))

(defprotocol Described
  (set-tooltip [this string])
  (get-tooltip [this]))

(defn assert-vector [n]
  (if (number? n) [n n] n))

(defn hudnode-code [node-name children-name parent-name focusable]
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
       this#)
     (set-parent
       [this# parent#]
        (reset! ~parent-name parent#)
        this#)
     (get-parent [this#] @~parent-name)
     (is-focusable [this#] ~focusable)))

(defn positioned-code [node-name]
  `(Positioned
     (set-position
       [this# pos#]
       (let [[x# y#] (assert-vector pos#)]
         (.setLocalTranslation
           ~node-name
           (Vector3f. x# (- y#) (.z (.getLocalTranslation ~node-name))))
         this#))
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

(defmacro defhudrecord [name args focusable & body]
  `(defrecord ~name ~args
     ~@(hudnode-code 'node 'children 'parent focusable)
     ~@body))

(defmacro defposrecord [name args focusable & body]
  `(defhudrecord ~name ~args ~focusable
     ~@(positioned-code (first args))
     ~@body))

(defhudrecord Screen [material font node input-manager asset-manager
                      geoms->elements event-queue start-fn
                      children parent]
  false
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue event-queue))
  cc/Lifecycle
  (start [this]
    (when start-fn (start-fn this)))
  (stop [this]))

(defposrecord ContainerElement [node children parent] false)

(defposrecord PictureElement [node geom tooltip-atom children parent] false
  Sized
  (set-size [this size]
    (let [[w h] (assert-vector size)]
      ;; We want the Quad to extend downwards
      (-> geom .getMesh (.updateGeometry w (- h) true))
      (.updateModelBound geom)
      this))
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
     ~@(forward-code 'Textual 'title)
     Sized
     ~'(set-size [this size]
         (let [[w h] (assert-vector size)]
           (set-size body size)
           (set-size header [w consts/header-height])
           (set-size title [w consts/header-height])))
     ~'(get-size [this] (get-size body))
     ~'(auto-size [this] (auto-size this 0))
     ~'(auto-size [this margin]
         (auto-size body margin)
         (let [w (get-width body)]
           (set-width header w)
           (set-width title w)))))

(def alignments {:center BitmapFont$Align/Center
                 :left BitmapFont$Align/Left
                 :right BitmapFont$Align/Right})

(def valignments {:top BitmapFont$VAlign/Top
                  :bottom BitmapFont$VAlign/Bottom
                  :vcenter BitmapFont$VAlign/Center})

(defposrecord TextElement [node geom children parent] false
  Sized
  (set-size [this size]
    (let [[w h] (assert-vector size)]
      ;; The box extends downwards.
      (.setBox node (Rectangle. 0 0 w h))
      this))
  (get-size [this]
    (-> node ((juxt #(.getLineWidth %) #(.getHeight %)))))
  (auto-size [this]
    (.setBox node nil)
    (let [w (.getLineWidth node)
          h (.getHeight node)]
      (set-size this [w h])))
  Textual
  (set-text [this text]
    (.setText node (str text))
    this)
  (set-font-size [this size]
    (.setSize node size)
    this)
  (set-alignment [this align]
    (if (align alignments)
      (.setAlignment node (align alignments))
      (.setVerticalAlignment node (align valignments)))))

(defn string-contains-any? [s & others]
  (some #(.contains s %) others))

(defmacro with-children-and-parent [& body]
  (with-gensyms [children parent]
    `(let [~children (atom #{})
           ~parent (atom nil)]
       ~@(walk/postwalk
           (fn [x]
             (if (and (seq? x)
                      (symbol? (first x))
                      (re-matches #"->(.+Element|Screen)" (name (first x))))
               (concat x [children parent])
               x))
           body))))

(defn new-node []
  (Node. (str (ccfns/get-new-id))))

(defn new-geom [mesh]
  (Geometry. (str (ccfns/get-new-id)) mesh))

(defmemoized get-material [screen texture-name]
  (let [{:keys [material asset-manager]} screen]
    (doto (.clone material)
      (.setTexture "ColorMap" (.loadTexture asset-manager texture-name)))))

(defn add-general-functionality
  [{:keys [geoms->elements]} element {:keys [clickable tooltip]}]
  (when clickable (swap! geoms->elements assoc (:geom element) element))
  (some->> tooltip (set-tooltip element)))

(defn set-element-defaults [element {:keys [pos size]}]
  (-> element
      (set-position (or pos 0))
      (set-size (or size 0))
      (set-depth 0.1)))

(defn create-element [screen {:keys [texture-name] :as options}]
  (with-children-and-parent
    (let [{:keys [material geoms->elements]} screen
          mesh (Quad. 0 0 true)
          geom (new-geom mesh)
          node (new-node)
          element (->PictureElement node geom (atom nil))]
      (set-material element
                    (if texture-name
                      (get-material screen texture-name)
                      (.clone material)))
      (.attachChild node geom)
      (set-element-defaults element options)
      (add-general-functionality screen element options)
      element)))

(defn create-text-element [screen options]
  (with-children-and-parent
    (let [{:keys [font geoms->elements]} screen
          node (BitmapText. font)
          geom (first (.getChildren node))
          element (->TextElement node geom)]
      (set-element-defaults element options)
      (add-general-functionality screen element options)
      element)))

(defn create-window [screen options]
  (let [hh consts/header-height
        node (new-node)
        body (create-element screen {:pos [0 hh]})
        header (create-element screen {:size [0 hh]})
        title (create-text-element screen {:size [0 hh]})
        element (->WindowElement node body header title)]
    (.setLineWrapMode (:node title) LineWrapMode/NoWrap)
    (.attachChild node (:node body))
    (.attachChild node (:node header))
    (.attachChild node (:node title))
    (set-alignment title :center)
    (set-element-defaults element options)
    element))

(defn create-container [pos]
  (with-children-and-parent
    (doto (->ContainerElement (new-node)) (set-position pos))))

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
  (with-children-and-parent
    (let [asset-manager (.getAssetManager app)
          input-manager (.getInputManager app)
          event-queue (ref [])
          node (new-node)
          start-fn (fn [screen]
                     (.setLocalTranslation
                       node (Vector3f. 0 consts/resolution-y 0))
                     (.setQueueBucket node RenderQueue$Bucket/Gui)
                     (.addRawInputListener
                       input-manager
                       (input-listener (make-click-callback screen)))
                     (.attachChild (.getGuiNode app) node))]
      (->Screen
        (doto (Material. asset-manager
                         "Common/MatDefs/Misc/Unshaded.j3md")
          (.. (getAdditionalRenderState)
              (setBlendMode RenderState$BlendMode/Alpha)))
        (.loadFont asset-manager "fonts/droid24_outline.fnt")
        node
        input-manager
        asset-manager
        (atom {})
        event-queue
        start-fn))))
