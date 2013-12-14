(defproject iris "0.1.0-SNAPSHOT"
  :description "iris: a graphics pipeline for pedagogy"
  :url "http://github.com/rogerallen/iris"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot iris.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
