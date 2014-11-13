(ns game.client.hud)

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

(javafx.application.Platform/setImplicitExit false)

(ns game.client.hud
  (:use game.utils)
  (:require (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [items :as items])
            [game.constants :as consts]
            [game.common.jme-utils :as ju]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:import (com.jme3x.jfx GuiManager AbstractHud)
           (com.jme3.texture.plugins AWTLoader)
           (com.jme3.texture Texture2D)
           (com.jme3.scene Node Geometry)
           (com.jme3.scene.shape Quad)
           (com.jme3.material Material)
           (com.jme3x.jfx.cursor ICursorDisplayProvider)
           (javafx.embed.swing SwingFXUtils)
           (javafx.event EventHandler EventType Event ActionEvent)
           (javafx.geometry Point3D Rectangle2D)
           (javafx.scene Scene)
           (javafx.scene.input MouseEvent MouseButton)
           (javafx.scene.control Button TextField Label LabelBuilder
                                 ProgressBar)
           (javafx.scene.image Image ImageView)
           (javafx.scene.layout Region Pane StackPane HBox VBox GridPane
                                TilePane)
           (javafx.scene.shape Arc ArcType Rectangle)
           (javafx.scene.transform Scale)
           (javafx.application Platform)))

(defmacro defsetter [name & methods]
  (with-gensyms [obj x y]
    `(defn ~name
       ([obj# arg#] (if (vector? arg#)
                      (~name obj# (arg# 0) (arg# 1))
                      (~name obj# arg# arg#)))
       ([~obj ~x ~y] (doto ~obj  ~@(for [m methods] (list m x y)))))))

(defsetter set-size .setMaxSize .setMinSize)

(defsetter relocate .relocate)

(defn gridpane-add [gridpane & children]
  (runmap (fn [[e x y]] (.add gridpane e x y)) children)
  gridpane)

(let [children-fn (fn [f] (fn [parent children]
                            (let [children-list (.getChildren parent)]
                              (runmap #(f children-list %) children)
                              parent)))]
  (def add-children (children-fn (memfn add child)))
  (def remove-children (children-fn (memfn remove child))))

(defn clear-children [parent]
  (-> parent .getChildren .clear))

(defmemoized load-image [path]
  (Image. path true))

(defmacro fx-run-later [& body]
  `(Platform/runLater (fn [] ~@body)))

(defmacro do-every-ms [last-time ms & body]
  `(let [curr-time# (current-time-ms)]
     (when (> curr-time# (+ ~last-time ~ms))
       ~@body)))

(defn mouse-event->button-int [e]
  (condp = (.getButton e)
    MouseButton/PRIMARY consts/mouse-left
    MouseButton/SECONDARY consts/mouse-right
    MouseButton/MIDDLE consts/mouse-middle))

(defmulti tree->jfx (fn [tree hud-state] (:type tree)))

(defmulti concrete-event (fn [abstract-event jfx-event] (:type abstract-event)))

(defmethod concrete-event :inv-click [abstract-event jfx-event]
  {:type :inv-click :pressed true
   :button (mouse-event->button-int jfx-event)
   :path (:path abstract-event)})

(defmethod concrete-event :destroy-item [abstract-event jfx-event]
  (dissoc abstract-event :trigger))

(def triggers-map
  {:pressed MouseEvent/MOUSE_PRESSED
   :clicked MouseEvent/MOUSE_CLICKED
   :action ActionEvent/ACTION
   :entered MouseEvent/MOUSE_ENTERED_TARGET
   :exited MouseEvent/MOUSE_EXITED_TARGET})

(defmacro event-handler [argsv & body]
  `(reify EventHandler (~'handle ~argsv ~@body)))

(defmacro add-event-handler [node trigger argsv & body]
  `(.addEventHandler
     ~node
     (triggers-map ~trigger)
     (event-handler ~argsv ~@body)))

(defn add-game-event-maker [node event {:keys [event-queue]}]
  (add-event-handler
    node (:trigger event) [this e]
    (ccfns/queue-conj
      event-queue
      (concrete-event event e)))
  node)

(defn add-tooltip-stuff [node hud-state tooltip]
  (let [ttpane (get-in hud-state [:panes :tooltip])
        tt {:type :label :id "tooltip" :text tooltip}
        add-handler (fn [trigger f]
                      (add-event-handler
                        node trigger [this e]
                        (when (= (.getTarget e) node)
                          (f))))]
    (add-handler
      :entered
      #(let [p (.localToScene node consts/icon-size 0)]
         (add-children ttpane [(doto (tree->jfx tt nil)
                                 (relocate (.getX p) (.getY p)))])))
    (add-handler
      :exited
      #(clear-children ttpane)))
  node)

(defn fix-common-things
  [node hud-state {:keys [pos size id children event classes tooltip]}]
  (cond-> node
    classes (doto (-> .getStyleClass (.addAll (into-array classes))))
    event (add-game-event-maker event hud-state)
    tooltip (add-tooltip-stuff hud-state tooltip)
    id (doto (.setId id))
    pos (relocate pos)
    size (set-size size)
    children (add-children (map #(tree->jfx % hud-state) children))))

(defmacro tree->jfx-method [class-name cargs kword]
  `(defmethod tree->jfx ~kword [~'tree ~'hud-state]
     (doto (new ~class-name ~@cargs)
       (fix-common-things ~'hud-state ~'tree))))

(defmethod tree->jfx :image [tree hud-state]
  (let [image (ImageView. (:texture tree))
        pane (Pane.)]
    (-> image .fitHeightProperty (.bind (.heightProperty pane)))
    (-> image .fitWidthProperty (.bind (.widthProperty pane)))
    (doto pane
      (add-children [image])
      (fix-common-things hud-state tree))))

(tree->jfx-method Label [(str (:text tree))] :label)

(tree->jfx-method Pane nil :pane)

(tree->jfx-method TilePane nil :tile-pane)

(tree->jfx-method ProgressBar [(:progress tree)] :progress-bar)

(tree->jfx-method HBox nil :hbox)

(tree->jfx-method VBox nil :vbox)

(tree->jfx-method Button [(str (:text tree))] :button)

(defn get-texture-name [item]
  (:icon (items/all-info item)))

(defn create-slot [item path]
  (let [children
        (cond
          (:quantity item) [{:type :label :text (:quantity item)
                             :id "stack-indicator" :size consts/icon-size}]
          (and (not item)
               (= :gear (path 0))) [{:type :label :text (name (path 1))
                                     :id "slot-description"
                                     :size consts/icon-size}]
          :else nil)]
    {:type :image :id "inv-slot" :size consts/icon-size
     :texture (if item (get-texture-name item) "inv_slot.png")
     :event {:type :inv-click :trigger :pressed :path path}
     :tooltip (when item (items/get-tooltip item))
     :children children}))

(defn get-map-order [game-state path]
  (if (= path [:gear])
    items/gear-slots-vector
    (keys (get-in game-state path))))

(defn items-paths [game-state path]
  (let [items (get-in game-state path)
        add-to-path (fn [endings] (map #(conj path %) endings))]
    (if (map? items)
      (let [order (get-map-order game-state path)]
        [(map #(% items) order) (add-to-path order)])
      [items (add-to-path (range (count items)))])))

(defn create-inventory [game-state path]
  (let [[items paths] (items-paths game-state path)
        slots (map create-slot items paths)
        id (if (= path [:gear]) "gear-pane" "inv-pane")
        container {:type :tile-pane :children slots :id id
                   :classes ["inventory"]}]
    container))

(defn create-inventories [hud-state game-state new-paths]
  (let [invs-pane (get-in hud-state [:panes :invs])
        nodes (map (comp #(tree->jfx % hud-state)
                         #(create-inventory game-state %))
                   new-paths)
        hashes (map #(hash (get-in game-state %)) new-paths)
        hud-changes #(add-children invs-pane nodes)]
    (runmap #(relocate % (or (get-in hud-state [:positions (%2 0)])
                             (get-in hud-state [:positions :other])))
            nodes new-paths)
    (-> (apply update-in hud-state [:invs] merge
               (map (fn [p n h]
                      [p {:node n :old-hash h}])
                    new-paths nodes hashes))
        (update-in [:jfx-changes] conj hud-changes))))

(defn remove-inventories [hud-state gone-paths]
  (let [invs-pane (get-in hud-state [:panes :invs])
        gone-nodes (map #(get-in hud-state [:invs % :node]) gone-paths)
        hud-changes #(remove-children invs-pane gone-nodes)]
    (-> (apply update-in hud-state [:invs] dissoc gone-paths)
        (update-in [:jfx-changes] conj hud-changes))))

(defn open-inventories [{:keys [looting inv-open?] :as game-state}]
  (set (cond-> (map #(vector :corpses % :drops) looting)
         inv-open? (conj [:inv] [:gear]))))

(defn existing-inventories [hud-state]
  (set (keys (:invs hud-state))))

(defn same-hash [[path {:keys [old-hash]}] game-state]
  (== (hash (get-in game-state path)) old-hash))

(defn new-and-gone-inventories [hud-state game-state]
  (let [x-same-hash (fn [x]
                      (set (keys (x #(same-hash % game-state)
                                    (:invs hud-state)))))
        not-changed (x-same-hash filter)
        changed (x-same-hash remove)
        existing (existing-inventories hud-state)
        open (open-inventories game-state)]
    [(set/difference open not-changed)
     (set/union changed (set/difference existing open))]))

(defn update-inventories [hud-state game-state]
  (let [[news gones] (new-and-gone-inventories hud-state game-state)]
    (-> hud-state
        (remove-inventories gones)
        (create-inventories game-state news))))

(defn update-mouse-slot-pos [{:keys [input-manager mouse-slot]}]
  (let [[x y] (->> input-manager ju/get-real-mouse-pos
                   (map #(+ % (/ consts/icon-size -2))))]
    (.setLocalTranslation
      mouse-slot x y (.z (.getLocalTranslation mouse-slot)))))

(defn create-mouse-texture
  [{:keys [mouse-texture-atom mtm-pane mtm-scene] :as hud-state} item]
  (when mtm-scene
    (reset! mouse-texture-atom :waiting)
    (fx-run-later
      (clear-children mtm-pane)
      (add-children mtm-pane
                    [(tree->jfx (dissoc (create-slot item nil) :event) nil)])
      (let [image (-> mtm-scene
                      (.snapshot nil)
                      (SwingFXUtils/fromFXImage nil))
            texture (Texture2D. (.load (AWTLoader.) image false))]
        (reset! mouse-texture-atom texture)))))

(defn update-mouse-slot-content [hud-state game-state]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state
        {:keys [mouse-texture-atom mouse-slot mouse-node]} hud-state
        item (cond-> (get-in game-state on-mouse)
               on-mouse-quantity (assoc :quantity on-mouse-quantity))]
    (if on-mouse
      (do
        (when (not @mouse-texture-atom)
          (create-mouse-texture hud-state item))
        (when (and (empty? (.getChildren mouse-slot))
                   (not= @mouse-texture-atom :waiting)
                   @mouse-texture-atom)
          (-> mouse-node .getMaterial (.setTexture "ColorMap"
                                                   @mouse-texture-atom))
          (.attachChild mouse-slot mouse-node)))
      (do (reset! mouse-texture-atom nil) (.detachAllChildren mouse-slot)))))

(defn update-mouse-slot [hud-state game-state]
  (update-mouse-slot-pos hud-state)
  (update-mouse-slot-content hud-state game-state))

(defn make-char-pane-tree [char]
  (let [{:keys [name hp max-hp level]} char
        w 170 nh 22 th 22 bh 12]
    {:type :pane
     :children
     [{:type :label :text name :id "name-label" :size [w nh]}
      {:type :label :text level :id "level-label"
       :pos [(- w (/ th 2)) -5] :size th}
      {:type :progress-bar :progress (/ hp max-hp) :id "hp-bar"
       :pos [0 (+ nh (/ (- th bh) 2))] :size [w bh]}
      {:type :label :text (str (int hp) "/" (int max-hp)) :id "hp-label"
       :pos [0 nh] :size [w th]}]}))

(defn hash-char [char]
  (hash (select-keys char [:level :hp :max-hp])))

(defn changed-char-panes [hud-state game-state ckeys ids]
  (let [keys-ids (map vector ckeys ids)
        chars (:chars game-state)
        changed (filterv (fn [[k i]]
                          (not= (get-in hud-state [:chars-state k :old-hash])
                                (hash-char (chars i))))
                        keys-ids)]
    changed))

(defn create-char-node [char pos]
  (when char
    (relocate (tree->jfx (make-char-pane-tree char) nil)
              (pos 0) (pos 1))))

(defn create-and-remove-panes [hud-state game-state changed]
  (let [chars (:chars game-state)
        chars-pane (get-in hud-state [:panes :chars])
        poses (:chars-poses hud-state)
        changed+nodes (map (fn [[k id :as ki]]
                             (conj ki (create-char-node (chars id) (poses k))))
                           changed)]
    (fx-run-later
      (remove-children chars-pane
                       (map #(get-in hud-state [:chars-state (first %) :node])
                            changed))
      (add-children chars-pane (remove nil? (map #(% 2) changed+nodes))))
    (reduce (fn [hs [k id node]]
              (assoc-in hs [:chars-state k]
                        {:node node :old-hash (hash-char (chars id))}))
            hud-state
            changed+nodes)))

(defn update-char-panes [hud-state game-state]
  (let [{:keys [own-id chars]} game-state
        changed (changed-char-panes
                       hud-state game-state
                       [:self :target]
                       [own-id (get-in chars [own-id :target])])]
    (create-and-remove-panes hud-state game-state changed)))

(defn make-destroy-dialog-tree [game-state]
  (let [{:keys [on-mouse on-mouse-quantity]} game-state
        omq on-mouse-quantity
        item (get-in game-state on-mouse)
        name (:name (items/all-info item))
        text (str "Destroy " name (when omq (str " (" omq ")")) "?")
        make-event (fn [a] {:type :destroy-item :trigger :action :destroy a})
        make-button (fn [t a] {:type :button :size [60 25] :text t
                               :event (make-event a)})]
    {:type :vbox
     :id "destroy-dialog"
     :children
     [{:type :label :text text}
      {:type :hbox
       :children
       [(make-button "Yes" true) (make-button "No" false)]}]}))

(defn create-destroy-item-dialog [hud-state game-state]
  (let [node (doto (tree->jfx (make-destroy-dialog-tree game-state) hud-state)
               (relocate 400 400))]
    (fx-run-later (add-children (get-in hud-state [:panes :chars]) [node]))
    (assoc-in hud-state [:dialogs :destroying-item] node)))

(defn remove-destroy-item-dialog [hud-state]
  (let [node (get-in hud-state [:dialogs :destroying-item])]
    (fx-run-later (remove-children (get-in hud-state [:panes :chars]) [node]))
    (dissoc-in hud-state [:dialogs :destroying-item])))

(defn update-destroy-dialog [hud-state game-state]
  (let [di :destroying-item
        destroying (di game-state)
        dialog (get-in hud-state [:dialogs di])]
    (cond
      (and destroying (not dialog))
      (create-destroy-item-dialog hud-state game-state)
      (and (not destroying) dialog)
      (remove-destroy-item-dialog hud-state)
      :else hud-state)))

(defn update-javafx [{:keys [jfx-changes] :as hud-state}]
  (fx-run-later (doseq [f jfx-changes] (f)))
  (assoc hud-state :jfx-changes []))

(deftype HudSystem [hud-state-atom gui-manager fxhud]
  cc/Lifecycle
  (start [this]
    (.attachHudAsync gui-manager fxhud))
  (stop [this]
    (.detatchHudAsync gui-manager (first (.getAttachedHuds gui-manager))))
  cc/EventsProducer
  (get-events [this]
    (ccfns/reset-queue (:event-queue @hud-state-atom)))
  cc/Updatable
  (update [this {:keys [game-state events]}]
    (update-mouse-slot @hud-state-atom game-state)
    (swap! hud-state-atom
           #(-> %
                (update-inventories game-state)
                (update-char-panes game-state)
                (update-destroy-dialog game-state)
                (update-javafx)))))

(defn random-string [length]
  (->> #(rand-nth "abcdefghijklmnopqrstuvxyz0123456789")
       (repeatedly length)
       (apply str)))

(defn random-rename [file]
  (let [rname (str "tmp/" (random-string 20) ".css")
        contents (slurp file)]
    (spit (str "assets/" rname) contents)
    rname))

(defn add-stylesheet [node]
  (-> node .getStylesheets (.add (random-rename "assets/gui_style.css"))))

(defn init-hud-system [app]
  (let [gui-node (.getGuiNode app)
        asset-manager (.getAssetManager app)
        input-manager (.getInputManager app)
        mouse-slot (doto (Node. "mouse-slot") (.setLocalTranslation 0 0 1))
        mouse-node (doto (Geometry.
                           "mouse-node"
                           (Quad. consts/icon-size consts/icon-size true))
                     (.setMaterial
                       (Material. asset-manager
                                  "Common/MatDefs/Misc/Unshaded.j3md")))
        mtm-pane (doto (Pane.) add-stylesheet)
        panes [(Pane.) (Pane.) (doto (Pane.) (.setMouseTransparent true))]
        panes-map (zipmap [:chars :invs :tooltip] panes)
        hud-state-atom (atom {:jfx-changes []
                              :panes panes-map
                              :chars-poses {:self [10 10] :target [200 10]}
                              :chars-state {:self {:old-hash (hash nil)}
                                            :target {:old-hash (hash nil)}}
                              :dialogs {}
                              :input-manager input-manager
                              :event-queue (ref [])
                              :mouse-texture-atom (atom nil)
                              :mtm-pane mtm-pane
                              :invs {}
                              :mouse-slot mouse-slot
                              :mouse-node mouse-node
                              :positions {:inv [700 100]
                                          :gear [800 100]
                                          :other [100 100]}})
        gui-manager (GuiManager.
                      gui-node asset-manager app false
                      (proxy [ICursorDisplayProvider] []
                        (setup [_])
                        (showCursor [_])))
        root-pane  (doto (Pane.)
                     add-stylesheet
                     (add-children panes))
        fxhud (proxy [AbstractHud] []
                (innerInit []
                  root-pane))]
    (fx-run-later (swap! hud-state-atom assoc :mtm-scene
                         (Scene. mtm-pane consts/icon-size consts/icon-size)))
    (.attachChild gui-node mouse-slot)
    (->HudSystem hud-state-atom gui-manager fxhud)))
