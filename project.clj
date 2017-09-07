(defproject game "0.1.0-SNAPSHOT"
  :description "An MMORPG."
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [robert/hooke "1.3.0"]
                 [seesaw "1.4.4"]
                 [org.clojars.oskarkv/kryonet "2.21.0-SNAPSHOT"]
                 [org.jmonkeyengine/jme3-core "3.1.0-stable"]
                 [org.jmonkeyengine/jme3-desktop "3.1.0-stable"]
                 [org.jmonkeyengine/jme3-lwjgl "3.1.0-stable"]
                 [org.clojars.oskarkv/JME3-JFX "2.184.2017-03-09-SNAPSHOT"]
                 ;[com.jme3x/jfx "2.184.2016-04-30_145140-ccbd413"]
                 [net.mikera/imagez "0.12.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.60.3"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [com.rpl/specter "1.0.1"]
                 [loco "0.3.1"]
                 [aysylu/loom "1.0.0"]]
  :resource-paths ["assets"]
  :repositories [["bintray" "https://jcenter.bintray.com"]
                 ["bintrayjme" "http://dl.bintray.com/jmonkeyengine/contrib"]]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
             "-Djavafx.animation.fullspeed=true"
             "-Dclojure.compiler.direct-linking=false"]
  :repl-options {:init (set! *print-length* 50)}
  :global-vars {*warn-on-reflection* false})
