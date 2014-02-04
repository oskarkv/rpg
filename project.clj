(defproject game "0.1.0-SNAPSHOT"
  :description "An MMORPG."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojars.miguno/kryonet-all "2.18"]
                 [org.clojars.oskarkv/jmonkeyengine "3.0.1-SNAPSHOT"]
                 [robert/hooke "1.3.0"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [tonegodgui/tonegodgui "0.1.0"]]
  :resource-paths ["assets"]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
  :global-vars {*warn-on-reflection* false})
