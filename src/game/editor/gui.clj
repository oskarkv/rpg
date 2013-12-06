(ns game.editor.gui
  (:import (java.awt Dimension EventQueue FlowLayout)
           (java.awt.event ActionListener ComponentListener WindowAdapter
                           WindowEvent WindowListener WindowStateListener)
           (javax.swing ImageIcon JButton JFrame
                        JMenu JMenuBar JMenuItem JPanel
                        JTextArea JToolBar SwingConstants))
  (:require (game.common [core :as cc])
            (game.editor [editor-functions :as e-fns])
            (clojure [reflect :as refl])))

(defmacro create-listener [type prefix]
  (let [methods (map :name (:members (refl/reflect (resolve type))))
        fns (map (fn [sym] (symbol (str prefix "-" sym))) methods)
        defs (map (fn [method fn]
                    (list method ['this 'event] (list fn 'this 'event)))
                  methods fns)]
    `(reify ~type ~@defs)))

(defn stop-application [app j-frame]
  (cc/stop app)
  (.dispose j-frame))

(defn start-j-frame [j-frame]
  (doto j-frame
    (.pack)
    (.setVisible true)))

(defn add-file-menu [app j-frame]
  (let [file-menu (JMenu. "File")
        open (JMenuItem. "Open")
        save (JMenuItem. "Save")
        exit (JMenuItem. "Exit")]
    (.addActionListener
      exit
      (reify ActionListener
        (actionPerformed [this event]
          (stop-application app j-frame))))
    (doto file-menu
      (.add open)
      (.add save)
      (.add exit))
    file-menu))

(defn create-tool-bar [editor-state-atom]
  (let [tool-bar (JToolBar. (JToolBar/VERTICAL))
        spawn-button (JButton. (ImageIcon. "assets/editor/toolbar/yellow.png"))
        b2 (JButton. (ImageIcon. "assets/editor/toolbar/red.png"))
        b3 (JButton. (ImageIcon. "assets/editor/toolbar/blue.png"))
        b4 (JButton. (ImageIcon. "assets/editor/toolbar/green.png"))]
    (.addActionListener
      spawn-button
      (reify ActionListener
        (actionPerformed [this event]
          (swap! editor-state-atom
                 conj
                 {:current-action e-fns/create-spawn-location}))))
    (doto tool-bar
      (.add spawn-button)
      (.add b2)
      (.add b3)
      (.add b4))
    tool-bar))

(defn create-menu-bar [app j-frame]
  (let [menu-bar (JMenuBar.)]
    (.add menu-bar (add-file-menu app j-frame))
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

(defn create-editor-swing-app [app editor-state-atom]
  (let [context (init-context app)
        canvas (.getCanvas context)
        j-frame (JFrame. "Editor")
        flow-layout (FlowLayout.)
        menu-bar (create-menu-bar app j-frame)
        main-panel (doto (JPanel. flow-layout)
                     (.add canvas)
                     (.add (create-tool-bar editor-state-atom)))]
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
          (componentMoved [this event])))
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
