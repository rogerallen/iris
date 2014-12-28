(defproject iris "0.1.0-SNAPSHOT"
  :description  "iris: a pedagogical clojure graphics pipeline"
  :url          "http://github.com/rogerallen/iris"
  :license      {:name "Eclipse Public License"
                 :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [primitive-math      "0.1.4"]]
  :main         ^:skip-aot iris.core
  :target-path  "target/%s"
  :jvm-opts     ^:replace [] ;; Enable full optimizer
  :profiles     {:uberjar {:aot :all}
                 ;; lein with-profile perf ...
                 :perf    {:plugins [[lein-nodisassemble "0.1.3"]]}})
