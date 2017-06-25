(in-ns 'game.client.hud)
(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(javafx.application.Platform/setImplicitExit false)

(ns game.client.hud
  (:require
   [clojure.set :as set]
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns]
   [game.common.jme-utils :as ju]
   [game.common.spells :as csp]
   [game.constants :as const]
   [game.hierarchies :as hier]
   [game.stats-and-items :as sni]
   [game.utils :refer :all])
  (:import
   (com.jme3.material Material)
   (com.jme3.scene Geometry Node)
   (com.jme3.scene.shape Quad)
   (com.jme3.texture.plugins AWTLoader)
   (com.jme3.texture Texture2D)
   (com.jme3x.jfx AbstractHud GuiManager)
   (com.jme3x.jfx.cursor ICursorDisplayProvider)
   (javafx.application Platform)
   (javafx.embed.swing SwingFXUtils)
   (javafx.event ActionEvent EventHandler)
   (javafx.scene.control Button Label ProgressBar)
   (javafx.scene.image Image ImageView)
   (javafx.scene.input MouseButton MouseEvent)
   (javafx.scene.layout HBox Pane TilePane VBox)
   (javafx.scene Scene)
   (javafx.scene.shape Arc ArcType Rectangle)))

(defmacro defsetter [name & methods]
  (with-gensyms [obj x y]
    `(defn ~name
       ;; use ensure-vec here instead
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
    MouseButton/PRIMARY const/mouse-left
    MouseButton/SECONDARY const/mouse-right
    MouseButton/MIDDLE const/mouse-middle))

(defmulti tree->jfx
  "Takes a description of a HUD element (a tree structure) in Clojure data, and
   creates the JavaFX objects to represent it."
  (fn [tree hud-state] (:type tree)))

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
     #(let [p (.localToScene node (double const/inv-icon-size) (double 0))]
        (add-children ttpane [(doto (tree->jfx tt nil)
                                (relocate (.getX p) (.getY p)))])))
    (add-handler
     :exited
     #(clear-children ttpane)))
  node)

(defn fix-common-things [node hud-state tree]
  (let [{:keys [pos size id children event classes tooltip]} tree]
    (cond-> node
      classes (doto (-> .getStyleClass (.addAll (into-array classes))))
      event (add-game-event-maker event hud-state)
      tooltip (add-tooltip-stuff hud-state tooltip)
      id (doto (.setId id))
      pos (relocate pos)
      size (set-size size)
      children (add-children (map #(tree->jfx % hud-state) children)))))

(defmacro tree->jfx-method
  "Converts a tree description to an object by creating an instance of
   class-name, passing to the constructor the cargs. The kword is the
   multimethod dispatch value."
  [class-name cargs kword]
  `(defmethod tree->jfx ~kword [~'tree ~'hud-state]
     (doto (new ~class-name ~@cargs)
       (fix-common-things ~'hud-state ~'tree))))

(defmethod tree->jfx :image [tree hud-state]
  (let [image (ImageView. (load-image (:texture tree)))
        pane (Pane.)]
    (-> image .fitHeightProperty (.bind (.heightProperty pane)))
    (-> image .fitWidthProperty (.bind (.widthProperty pane)))
    (doto pane
      (add-children [image])
      (fix-common-things hud-state tree))))

(defmethod tree->jfx :arc [tree hud-state]
  (let [r (:size tree)
        c (:center tree)
        arc (doto (Arc. c c r r 90 360)
              (.setType ArcType/ROUND)
              (fix-common-things hud-state (dissoc tree :size)))]
    arc))

(tree->jfx-method Label [(str (:text tree))] :label)

(tree->jfx-method Pane nil :pane)

(tree->jfx-method TilePane nil :tile-pane)

(tree->jfx-method ProgressBar [(:progress tree)] :progress-bar)

(tree->jfx-method HBox nil :hbox)

(tree->jfx-method VBox nil :vbox)

(tree->jfx-method Button [(str (:text tree))] :button)

(defn get-texture-name [item]
  (:icon (sni/all-info item)))

(defn create-slot [item path]
  (let [children
        (cond
          (:quantity item) [{:type :label :text (:quantity item)
                             :id "stack-indicator" :size const/inv-icon-size}]
          (and (not item)
               (= :gear (path 0))) [{:type :label :text (name (path 1))
                                     :id "slot-description"
                                     :size const/inv-icon-size}]
          :else nil)]
    {:type :image :id "inv-slot" :size const/inv-icon-size
     :texture (if item (get-texture-name item) "inv_slot.png")
     :event {:type :inv-click :trigger :pressed :path path}
     :tooltip (when item (sni/get-tooltip item))
     :children children}))

(defn get-map-order [game-state path]
  (if (= path [:gear])
    hier/gear-slots-vector
    (keys (get-in game-state path))))

(defn items-paths
  "Returns a seq of [item path] pairs, from the given path in the game-state.
   If the original collection was not a map, the order of the returned seq will
   be the same as the original. If it the original was a map, the order is
   defined by get-map-order."
  [game-state path]
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

(defn create-inventories
  "Returns a new hud-state, with new inventories, based on what's in the
   game-state at paths. Also attachs the new inventories to the gui-node, as
   children to the invs pane."
  [hud-state game-state paths]
  (let [invs-pane (get-in hud-state [:panes :invs])
        nodes (map (comp #(tree->jfx % hud-state)
                         #(create-inventory game-state %))
                   paths)
        hashes (domap #(hash (get-in game-state %)) paths)]
    (runmap #(relocate % (or (get-in hud-state [:positions (%2 0)])
                             (get-in hud-state [:positions :other])))
            nodes paths)
    (fx-run-later (add-children invs-pane nodes))
    (apply update hud-state :invs merge
           (map (fn [p n h]
                  [p {:node n :old-hash h}])
                paths nodes hashes))))

(defn remove-inventories [hud-state paths]
  (let [invs-pane (get-in hud-state [:panes :invs])
        gone-nodes (map #(get-in hud-state [:invs % :node]) paths)]
    (fx-run-later (remove-children invs-pane gone-nodes))
    (apply update hud-state :invs dissoc paths)))

(defn open-inventories [{:keys [looting inv-open?] :as game-state}]
  (set (cond-> (map #(vector :corpses % :drops) looting)
         inv-open? (conj [:inv] [:gear]))))

(defn existing-inventories [hud-state]
  (set (keys (:invs hud-state))))

(defn same-hash [[path {:keys [old-hash]}] game-state]
  (= (hash (get-in game-state path)) old-hash))

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
                   (map #(+ % (/ const/inv-icon-size -2))))]
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
  (let [{:keys [name hp max-hp mana max-mana level]} char
        w 170 nh 22 th 22 bh 12
        bar-with-text
        (fn [ypos pool pool-max name-prefix]
          [{:type :progress-bar :progress (/ pool pool-max)
            :id (str name-prefix "-bar")
            :pos [0 (+ ypos (/ (- th bh) 2))] :size [w bh]}
           {:type :label :text (str (int pool) "/" (int pool-max))
            :id (str name-prefix "-label")
            :pos [0 ypos] :size [w th]}])]
    {:type :pane
     :id "char-pane"
     :children
     (concat
      [{:type :label :text name :id "name-label" :size [w nh]}
       {:type :label :text level :id "level-label"
        :pos [(- w (/ th 2)) -5] :size th}]
      (bar-with-text nh hp max-hp "hp")
      (when mana (bar-with-text (+ nh bh) mana max-mana "mana")))}))

(defn hash-char [char]
  (hash (select-keys char [:level :hp :max-hp :mana :max-mana])))

(defn changed-char-panes [hud-state game-state ckeys ids]
  (let [keys-ids (zip ckeys ids)
        chars (:chars game-state)]
    (filterv (fn [[k i]]
               (not= (get-in hud-state [:chars-state k :old-hash])
                     (hash-char (chars i))))
             keys-ids)))

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
      (add-children chars-pane (keep #(% 2) changed+nodes)))
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
        name (:name (sni/all-info item))
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

(def icon-table
  {:regrowth "spells/heal.png"
   :empty "spells/empty.png"
   :unknown "spells/unknown.png"})

(defn create-spellbar-trees [spells]
  {:bar
   {:type :hbox
    :id "spell-bar"}
   :images
   (for [spell spells]
     {:type :image
      :size const/spell-icon-size
      :texture (or ((or spell :empty) icon-table)
                   (:unknown icon-table))})
   :arcs (repeat (count spells) {:type :arc :size const/spell-icon-size
                                 :center (/ const/spell-icon-size 2)
                                 :classes ["cooldown-indicator"]})})

(defn create-spellbar [spells]
  (let [{:keys [bar images arcs]} (create-spellbar-trees spells)
        make-nodes (fn [trees] (map #(tree->jfx % nil) trees))
        image-nodes (make-nodes images)
        arc-nodes (make-nodes arcs)
        bar-node (add-children (tree->jfx bar nil) image-nodes)]
    (runmap (fn [a i]
              (.setClip a (Rectangle. const/spell-icon-size
                                      const/spell-icon-size))
              (add-children i [a]))
            arc-nodes image-nodes)
    (relocate bar-node
              (- (/ const/resolution-x 2)
                 (* const/spell-icon-size (/ const/spell-slots 2)))
              (- const/resolution-y (+ const/spell-icon-size 10)))
    {:bar bar-node
     :arcs arc-nodes}))

(defn spell-list [game-state]
  (map :spell (:spells game-state)))

(defn spellbar-needs-update? [hud-state game-state]
  (not= (hash (spell-list game-state))
        (get-in hud-state [:spellbar :old-hash])))

(defn cooldown-left-quota [last-cast cd curr-time]
  (if (and cd (pos? cd))
    (max 0 (/ (+ last-cast (* 1000 cd) (- curr-time))
              (* 1000 cd)))
    0))

(defn update-cooldowns [hud-state game-state]
  (let [spells (:spells game-state)
        last-casts (map :last-cast spells)
        curr-time (current-time-ms)
        cds (map #(get-in csp/spells [% :cooldown]) (map :spell spells))]
    (fx-run-later
      (runmap (fn [arc lc cd]
                (.setLength arc (* 360 (cooldown-left-quota lc cd curr-time))))
              (get-in hud-state [:spellbar :arcs])
              last-casts
              cds))))

(defn update-spellbar [hud-state game-state]
  (let [old-node (get-in hud-state [:spellbar :node])
        new-hud-state
        (if (spellbar-needs-update? hud-state game-state)
          (let [{:keys [bar arcs]} (create-spellbar (spell-list game-state))]
            (fx-run-later (-> (get-in hud-state [:panes :chars])
                            (remove-children [old-node])
                            (add-children [bar])))
            (-> hud-state
              (assoc-in [:spellbar :node] bar)
              (assoc-in [:spellbar :arcs] arcs)
              (assoc-in [:spellbar :old-hash]
                        (hash (spell-list game-state)))))
          hud-state)]
    (update-cooldowns new-hud-state game-state)
    new-hud-state))

(deftype HudSystem [hud-state-atom gui-manager fxhud]
  cc/Lifecycle
  (start [this]
    (.attachHudAsync gui-manager fxhud))
  (stop [this]
    (.detachHudAsync gui-manager (first (.getAttachedHuds gui-manager))))
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
              (update-spellbar game-state)
              (update-destroy-dialog game-state)))))

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
        ;; The mouse slot should be displayed in front of the rest of the HUD,
        ;; which is at -1.
        mouse-slot (doto (Node. "mouse-slot") (.setLocalTranslation 0 0 1))
        mouse-node (doto (Geometry.
                          "mouse-node"
                          (Quad. const/inv-icon-size const/inv-icon-size true))
                     (.setMaterial
                      (Material. asset-manager
                                 "Common/MatDefs/Misc/Unshaded.j3md")))
        mtm-pane (doto (Pane.) add-stylesheet)
        panes [(Pane.) (Pane.) (doto (Pane.) (.setMouseTransparent true))]
        panes-map (zipmap [:chars :invs :tooltip] panes)
        hud-state-atom (atom {:panes panes-map
                              :chars-poses {:self [10 10] :target [200 10]}
                              :chars-state {:self {:old-hash nil}
                                            :target {:old-hash nil}}
                              :spellbar {:node nil :arcs nil :old-hash nil}
                              :dialogs {}
                              :input-manager input-manager
                              :event-queue (ref [])
                              :mouse-texture-atom (atom nil)
                              ;; Mouse texture maker things, not part of the
                              ;; GUI themselves, only for making the mouse slot
                              ;; texture.
                              :mtm-pane mtm-pane
                              :mtm-scene nil
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
    (fx-run-later
      (swap! hud-state-atom assoc :mtm-scene
             (Scene. mtm-pane const/inv-icon-size const/inv-icon-size)))
    (.attachChild gui-node mouse-slot)
    (->HudSystem hud-state-atom gui-manager fxhud)))
