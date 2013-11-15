(defproject iris "0.1.0-SNAPSHOT"
  :description "iris: a graphics pipeline for pedagogy"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.mikera/core.matrix "0.15.0"]]
  :main ^:skip-aot iris.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
