(defproject game "0.1.0-SNAPSHOT"
  :description "An MMORPG."
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [robert/hooke "1.3.0"]
                 [seesaw "1.4.4"]
                 [org.clojars.oskarkv/kryonet "2.21.0-SNAPSHOT"]
                 [org.clojars.oskarkv/jmonkeyengine "3.0.0"]
                 [jme3jfx "0.1.0-SNAPSHOT"]]
  :resource-paths ["assets"]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
             "-Djavafx.animation.fullspeed=true"]
  :global-vars {*warn-on-reflection* false})
