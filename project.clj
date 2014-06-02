(defproject uber "0.1.0-SNAPSHOT"
  :description "Uber optimization model: evolutionary algorithm "
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojars.difranco/netlogo "5.0"]                 
                 [org.scala-lang/scala-library "2.9.1"]]
  :resource-paths ["lib/*"]
  :aot [uber.core]
  :main uber.core)
