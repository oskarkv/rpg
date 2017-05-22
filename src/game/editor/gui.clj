(ns game.editor.gui
  (:import (java.awt Dimension EventQueue FlowLayout Image)
           (java.awt.event ActionListener ComponentListener WindowAdapter
                           WindowEvent WindowListener WindowStateListener)
           (javax.swing ImageIcon JButton JFrame JLayeredPane JTextField
                        JMenu JMenuBar JMenuItem JPanel JLabel JWindow JDialog
                        JTextArea JToolBar SwingConstants))
  (:require (game [constants :as consts])
            (game.common [core :as cc])
            (game.editor [editor-functions :as e-fns])
            (clojure [reflect :as refl])
            (seesaw [core :as ssw-c]
                    [mig :as ssw-mig])))

(defn stop-application [app j-frame]
  (cc/stop app)
  (.dispose j-frame))

(defn start-j-frame [j-frame]
  (doto j-frame
    (.pack)
    (.setVisible true)))

(defn create-file-menu [event-queue]
  (let [file-menu
        (ssw-c/menu
         :text "File"
         :items
         [(ssw-c/menu-item
           :text "Open zone"
           :listen [:action-performed
                    (fn [e]
                      (.add event-queue {:type :load-zone}))])
          (ssw-c/menu-item
           :text "Save zone"
           :listen [:action-performed
                    (fn [e]
                      (.add event-queue {:type :save-zone}))])])]
    file-menu))

(defn create-zone-menu [event-queue]
  (let [zone-menu
        (ssw-c/menu
         :text "Zone"
         :items
         [(ssw-c/menu-item
           :text "Edit mobs"
           :listen [:action-performed
                    (fn [e]
                      (.add event-queue {:type :edit-zone}))])])]
    zone-menu))

(defn create-tool-bar [game-state-atom]
  (let [toolbar-bg (ssw-c/button-group)
        tool-bar
        (ssw-c/toolbar
         :orientation :vertical
         :items
         [(ssw-c/button
           :id "spawn-button"
           :icon (ImageIcon. consts/spawn-button-icon)
           :group toolbar-bg
           :listen
           [:action-performed
            (fn [e]
              (swap! game-state-atom
                     conj {:current-action e-fns/create-spawn-location}))])
          (ssw-c/button
           :icon (ImageIcon. (str consts/editor-toolbar "yellow.png"))
           :group toolbar-bg)])]
    tool-bar))

(defn create-menu-bar [app game-state-atom event-queue]
  (let [menu-bar (JMenuBar.)]
    (doto menu-bar
      (.add (create-file-menu event-queue))
      (.add (create-zone-menu event-queue)))
    menu-bar))

(defn calc-canvas-dim [dim]
  (let [canv-dim (.getSize dim)]
    (.setSize canv-dim (* 0.8 (.getWidth dim)) (* 0.8 (.getHeight dim)))
    canv-dim))

(defn resize-canvas [canvas dim]
  (.setPreferredSize canvas (calc-canvas-dim dim)))

(defn init-context [app]
  (let [context (.getContext app)]
    (.setSystemListener context app)
    (.setPreferredSize (.getCanvas context) (Dimension. 640 480))
    context))

(defn add-frame-listeners [frame canvas app]
  (ssw-c/listen
   frame
    ;;; Component Listeners
   :component-hidden  (fn [e])
   :component-moved   (fn [e])
   :component-resized (fn [e] (resize-canvas canvas frame))
   :component-shown   (fn [e])
   :window-closing    (fn [e] (stop-application app (.getSource e)))))

(defn create-add-mob-window [parent]
  (ssw-c/custom-dialog
   :title "Add zone mob"
   :content "Blargh!"
   :size [480 :by 320]
   :parent parent
   :visible? true
   :modal? true))

(defn create-edit-zone-window [j-frame game-map]
  (let [mob-types (vals (:mob-types game-map))
        add-icon (ImageIcon. consts/add-icon)
        delete-icon (ImageIcon. consts/delete-icon)
        button-size "h 24!, w 24!"
        list-size "h 100:200:300, w 80:120:150"
        zone-mobs (ssw-c/listbox :model (map :name mob-types))
        hp-label (ssw-c/label "HP: ")
        dmg-label (ssw-c/label "DMG: ")
        speed-label (ssw-c/label "Speed: ")
        mob-hp (ssw-c/label)
        mob-dmg (ssw-c/label)
        mob-speed (ssw-c/label)
        mob-panel (ssw-mig/mig-panel
                   :items [[hp-label]
                           [mob-hp "wrap"]
                           [dmg-label]
                           [mob-dmg "wrap"]
                           [speed-label]
                           [mob-speed "wrap"]])
        drops (ssw-c/listbox)
        groups (ssw-c/listbox)
        add-mob (ssw-c/button
                 :icon add-icon
                 :listen
                 [:action-performed (fn [e] (create-add-mob-window j-frame))])
        del-mob (ssw-c/button :icon delete-icon)
        add-drop (ssw-c/button :icon add-icon)
        del-drop (ssw-c/button :icon delete-icon)
        add-group (ssw-c/button :icon add-icon)
        del-group (ssw-c/button :icon delete-icon)
        main-panel
        (ssw-mig/mig-panel
         :items [[zone-mobs list-size]
                 [mob-panel]
                 [drops list-size]
                 [groups (str list-size ", wrap")]
                 [add-mob (str button-size ", split 2, span 2")]
                 [del-mob (str button-size)]
                 [add-drop (str button-size ", split 2")]
                 [del-drop (str button-size)]
                 [add-group (str button-size ", split 2")]
                 [del-group (str button-size)]])]
    (ssw-c/listen zone-mobs :selection
                  (fn [e]
                    (let [selected (ssw-c/selection zone-mobs)
                          mob (first (filter #(= selected (:name %))
                                             mob-types))]
                      (ssw-c/text! mob-hp (:hp mob))
                      (ssw-c/text! mob-speed (:speed mob))
                      (ssw-c/text! mob-dmg (:dmg mob)))))
    (ssw-c/custom-dialog
     :title "Edit zone"
     :content main-panel
     :size [800 :by 600]
     :parent j-frame
     :visible? true
     :modal? true)))

(defn create-editor-swing-app [app game-state-atom event-queue]
  (let [context (init-context app)
        canvas (.getCanvas context)
        menu-bar (create-menu-bar app game-state-atom event-queue)
        main-panel (ssw-mig/mig-panel
                    :constraints ["wrap 2"]
                    :items [[canvas]
                            [(create-tool-bar game-state-atom)]])
        j-frame (ssw-c/frame
                 :title "Editor"
                 :minimum-size [1200 :by 900]
                 :menubar menu-bar
                 :content main-panel
                 :on-close :nothing)]
    (add-frame-listeners j-frame canvas app)
    (doto j-frame
      (.addWindowStateListener
       (reify WindowStateListener
         (windowStateChanged [this event]
           (resize-canvas canvas (.getComponent event))))))
    (extend-type JFrame
      cc/Lifecycle
      (start [this]
        (EventQueue/invokeLater
         (fn []
           (start-j-frame this))))
      (stop [this]))
    (defn edit-zone []
      (EventQueue/invokeLater
       (fn []
         (create-edit-zone-window j-frame (:game-map @game-state-atom)))))
    j-frame))
