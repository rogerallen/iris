(ns iris.core
  (:require [clojure.pprint :as pp]
            [iris.pipeline :as iris])
  (:gen-class))

;; TODO
;; o geom/sphere
;; o add normals & lighting
;; o with-state+framebuffer?  that seems like a good thing.
;; o output image file

(defn run []
  ;; while true, with-camera? with-frame?
  (let [objects [{:type     :triangle-list
                  :vertices [{:x 0.0 :y 0.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                             {:x 1.0 :y 0.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                             {:x 1.0 :y 1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}

                             {:x -1.0 :y -1.0 :z 0.5 :r 1.0 :g 0.0 :b 0.0}
                             {:x 0.5 :y 0.0 :z 0.5 :r 0.0 :g 1.0 :b 0.0}
                             {:x 0.5 :y 1.0 :z 0.5 :r 0.0 :g 0.0 :b 1.0}
                             ]}]]
    (iris/draw objects)))

;;(println (run))

(defn -main
  [& args]
  (let [debug-results (doall (run))]
    ;;(pp/pprint debug-results)
    (iris/print-ppm)))
