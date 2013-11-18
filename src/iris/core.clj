(ns iris.core
  (:require [clojure.core.matrix :as mat]
            [clojure.pprint :as pp]
            [iris.pipeline :as iris])
  (:gen-class))

;; TODO
;; o speed!
;; o add more geometry
;; o add normals & lighting
;; o perspective matrix test
;; o texturing
;; o input scenes
;; o output image file

(defn round255 [x]
  (int (Math/floor (+ 0.5 (* 255 x)))))

(defn print-ppm
  "assume framebuffers are sequential in y"
  [framebuffers]
  (let [iw (:width @(first framebuffers))
        ih (* (count framebuffers) (:height @(first framebuffers)))]
    (println "P3" iw ih 255)
    (doseq [framebuffer framebuffers]
      (let [w (:width @framebuffer)
            h (:height @framebuffer)]
        (doseq [y (range h)]
          (do
            (doseq [x (range w)]
              (let [i (int (+ (* y w) x))
                    r (round255 (:r ((:data @framebuffer) i)))
                    g (round255 (:g ((:data @framebuffer) i)))
                    b (round255 (:b ((:data @framebuffer) i)))]
                (print r g b " ")))
            (println)))))))

(defn run [[viewport-x viewport-y viewport-width viewport-height]
           [framebuffer-x framebuffer-y framebuffer-width framebuffer-height]]
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
        state {:viewport [viewport-x viewport-y viewport-width viewport-height]
               :fbport [framebuffer-x framebuffer-y framebuffer-width framebuffer-height]
               :depth-range [0.0 1.0]
               ;; manipulate via gluLookAt
               :view-matrix (mat/identity-matrix 4)
               ;; manipulate via glScale/Rotate/Translate
               :model-matrix (mat/identity-matrix 4)
               ;; object coords v = [x y z w]^T (column)
               ;; eye-coords = model * view * v
               ;; manipulate via glFrustum/Ortho
               :projection-matrix (mat/identity-matrix 4)
               ;; clip-coords = projection * eye-coord
               }
        framebuffer {:x      framebuffer-x
                     :y      framebuffer-y
                     :width  framebuffer-width
                     :height framebuffer-height
                     :data   (vec (repeat (* framebuffer-width framebuffer-height)
                                          {:r 0 :g 0 :b 0 :z 1}))
                     }
        ]
    (iris/render-framebuffer state framebuffer objects)))

(defn -main
  [& args]
  ;; each of these seem to take about 25 seconds.  That's dissapointing

  ;; 1 thread
  ;;(print-ppm [(future (run [0 0 320 320] [0   0 320 320]))])

  ;; 2 threads
  (print-ppm [(future (run [0 0 320 320] [0   0 320 160]))
              (future (run [0 0 320 320] [0 160 320 160]))])

  ;; 4 threads
  ;;(print-ppm [(future (run [0 0 320 320] [0   0 320 80]))
  ;;            (future (run [0 0 320 320] [0  80 320 80]))
  ;;            (future (run [0 0 320 320] [0 160 320 80]))
  ;;            (future (run [0 0 320 320] [0 240 320 80]))])

  (shutdown-agents) ;; !! important when you use futures !!

)
