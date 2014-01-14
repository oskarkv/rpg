(ns game.editor.gui
  (:import (java.awt Dimension EventQueue FlowLayout)
           (java.awt.event ActionListener ComponentListener WindowAdapter
                           WindowEvent WindowListener WindowStateListener)
           (javax.swing ImageIcon JButton JFrame
                        JMenu JMenuBar JMenuItem JPanel
                        JTextArea JToolBar SwingConstants))
  (:require (game [constants :as consts])
            (game.common [core :as cc])
            (game.editor [core :as ec]
                         [editor-functions :as e-fns])
            (clojure [reflect :as refl])))

(defn stop-application [app j-frame]
  (cc/stop app)
  (.dispose j-frame))

(defn start-j-frame [j-frame]
  (doto j-frame
    (.pack)
    (.setVisible true)))

(defn add-file-menu [app j-frame game-state-atom]
  (let [file-menu (JMenu. "File")
        open (JMenuItem. "Open")
        save (JMenuItem. "Save")
        exit (JMenuItem. "Exit")]
    (.addActionListener
      exit
      (reify ActionListener
        (actionPerformed [this event]
          (stop-application app j-frame))))
    (.addActionListener
      save
      (reify ActionListener
        (actionPerformed [this event]
          (ec/enqueue-event {:type :save-zone}))))
    (doto file-menu
      (.add open)
      (.add save)
      (.add exit))
    file-menu))

(defn create-tool-bar [game-state-atom]
  (let [tool-bar (JToolBar. (JToolBar/VERTICAL))
        spawn-button
        (JButton. (ImageIcon. (str consts/spawn-button)))
        b2 (JButton. (ImageIcon. (str consts/editor-toolbar "red.png")))
        b3 (JButton. (ImageIcon. (str consts/editor-toolbar "blue.png")))
        b4 (JButton. (ImageIcon. (str consts/editor-toolbar "green.png")))]
    (.addActionListener
      spawn-button
      (reify ActionListener
        (actionPerformed [this event]
          (swap! game-state-atom
                 conj
                 {:current-action e-fns/create-spawn-location}))))
    (doto tool-bar
      (.add spawn-button)
      (.add b2)
      (.add b3)
      (.add b4))
    tool-bar))

(defn create-menu-bar [app j-frame game-state-atom]
  (let [menu-bar (JMenuBar.)]
    (.add menu-bar (add-file-menu app j-frame game-state-atom))
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

(defn create-editor-swing-app [app game-state-atom]
  (let [context (init-context app)
        canvas (.getCanvas context)
        j-frame (JFrame. "Editor")
        flow-layout (FlowLayout.)
        menu-bar (create-menu-bar app j-frame game-state-atom)
        main-panel (doto (JPanel. flow-layout)
                     (.add canvas)
                     (.add (create-tool-bar game-state-atom)))]
    (doto j-frame
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.addWindowListener
        (proxy [WindowAdapter] []
          (windowClosing [event]
            (stop-application app (.getSource event)))))
      (.add main-panel)
      (.setJMenuBar menu-bar)
      (.addComponentListener
        (reify ComponentListener
          (componentResized [this event]
            (resize-canvas canvas (.getComponent event)))
          ;; Empty implementation(s) to prevent error message when starting app
          (componentMoved [this event])
          (componentHidden [this event])
          (componentShown [this event])))
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
  j-frame))
