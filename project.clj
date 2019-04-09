(defproject game "0.1.0-SNAPSHOT"
  :description "An MMORPG."
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/data.priority-map "0.0.10"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [robert/hooke "1.3.0"]
                 ;;[seesaw "1.4.4"]
                 [org.clojars.oskarkv/kryonet "2.21.0-SNAPSHOT"]
                 [org.jmonkeyengine/jme3-core "3.1.0-stable"]
                 [org.jmonkeyengine/jme3-desktop "3.1.0-stable"]
                 [org.jmonkeyengine/jme3-lwjgl "3.1.0-stable"]
                 ;;[org.clojars.oskarkv/JME3-JFX "2.184.2017-03-09-SNAPSHOT"]
                 [com.jme3x/jfx "2.184.2016-04-30_145140-ccbd413"]
                 [net.mikera/imagez "0.12.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [com.rpl/specter "1.1.2"]
                 [loco "0.3.1"]
                 [aysylu/loom "1.0.2"]
                 [org.openjfx/javafx-controls "11.0.2"]]
  :resource-paths ["assets"]
  :repositories [["bintray" "https://jcenter.bintray.com"]
                 ["bintrayjme" "https://dl.bintray.com/jmonkeyengine/contrib"]]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
             "-Djavafx.animation.fullspeed=true"
             "-Dclojure.compiler.direct-linking=false"]
  :repl-options {:init (set! *print-length* 50)}
  :global-vars {*warn-on-reflection* false})
