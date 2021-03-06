(ns iris.core
  (:require [clojure.pprint :as pp]
            [clojure.edn :as edn]
            [iris.matrix :as mat]
            [iris.pipeline :as iris]
            [iris.util :as util])
  (:gen-class))

;; TODO
;; o user vertex/pixel shader functions
;; o texturing
;; o objects should allow "commands" in-band to modify state

(defn run [n width height]
  (let [objects [{:type     :triangle-list
                  :vertices [{:x  0.0 :y  0.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                             {:x  1.0 :y -1.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                             {:x  1.0 :y  1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}

                             {:x -0.5 :y  0.0 :z 0.5 :r 1.0 :g 0.0 :b 0.0}
                             {:x  0.5 :y -0.8 :z 0.5 :r 0.0 :g 1.0 :b 0.0}
                             {:x  0.5 :y  0.8 :z 0.5 :r 0.0 :g 0.0 :b 1.0}

                             {:x -1.5 :y  0.0 :z 0.25 :r 1.0 :g 0.0 :b 0.0}
                             {:x -0.3 :y -1.2 :z 0.25 :r 0.0 :g 1.0 :b 0.0}
                             {:x -0.3 :y  1.2 :z 0.25 :r 0.0 :g 0.0 :b 1.0}
                             ]}]
        state {:viewport (double-array [0 0 width height])
               :fbport (double-array [0 0 width height])
               :depth-range (double-array [0.0 1.0])
               ;; manipulate via gluLookAt
               :view-matrix (mat/identity-matrix)
               ;; manipulate via glScale/Rotate/Translate
               :model-matrix (mat/identity-matrix)
               ;; object coords v = [x y z w]^T (column)
               ;; eye-coords = model * view * v
               ;; manipulate via glFrustum/Ortho
               :projection-matrix (mat/identity-matrix)
               ;; clip-coords = projection * eye-coord
               }
        framebuffer {:x      0
                     :y      0
                     :width  width
                     :height height
                     ;; :data allocated by parallel-render
                     }
        ]
    (iris/parallel-render-framebuffer n state framebuffer objects)))


(defn contains-in?
  [coll ks]
  (let [k (first ks)]
    (if (contains? coll k)
      (if (empty? (rest ks))
        true
        (contains-in? (get coll k) (rest ks)))
      false)))

(defn update-in-if
  [coll ks fn]
  (if (contains-in? coll ks)
    (update-in coll ks fn)
    coll))

(defn read-config
  "read configuration file & convert a few fields to the proper types"
  [edn-file]
  (-> (edn/read-string (slurp edn-file))
      (update-in-if [:state :viewport] double-array)
      (update-in-if [:state :fbport] double-array)
      (update-in-if [:state :light-vector] double-array)
      (update-in-if [:state :depth-range] double-array)
      (update-in-if [:state :view-matrix] double-array)
      (update-in-if [:state :model-matrix] double-array)
      (update-in-if [:state :projection-matrix] double-array)))

(defn -main
  [& args]
  (if (= 1 (count args))
    ;; read config from edn file
    (let [edn-file    (first args)
          _           (binding [*out* *err*]
                        (println "Rendering from" edn-file))
          config      (read-config edn-file)
          ;;_ (pp/pprint config)
          n           (:parallelism config)
          state       (:state config)
          framebuffer (:framebuffer config)
          objects     (:objects config)]
      (util/print-fb-to-ppm
       (iris/parallel-render-framebuffer n state framebuffer objects))
      (binding [*out* *err*]
        (println "done")))
    ;; else we have a pre-canned version of example1.edn
    (util/print-fb-to-ppm (run 4 320 320)))
  (shutdown-agents) ;; !! important when you use futures !!
)
