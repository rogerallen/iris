(ns iris.core
  (:refer-clojure :exclude [* - + == /]) ; get from core.matrix
  (:use clojure.core.matrix)
  ;;(:use clojure.core.matrix.operators)
  (:gen-class))

(defonce state (atom {:framebuffer-width 320
                      :framebuffer-height 240
                      :viewport-width 320
                      :viewport-height 240
                      :depth-range [0.0 1.0]
                      ;; manipulate via gluLookAt
                      :view-matrix (identity-matrix 4)
                      ;; manipulate via glScale/Rotate/Translate
                      :model-matrix (identity-matrix 4)
                      ;; object coords v = [x y z w]^T (column)
                      ;; eye-coords = model * view * v
                      ;; manipulate via glFrustum/Ortho
                      :projection-matrix (identity-matrix 4)
                      ;; clip-coords = projection * eye-coord
                      }))

(defonce framebuffer (atom (vec (repeat (clojure.core/*
                                         (@state :framebuffer-width)
                                         (@state :framebuffer-height))
                                        {:r 0 :g 0 :b 0}))))

(defn evaluate
  "given an object, evaluate it to decompose it into triangles"
  [objects]
  (for [o objects]
    (do
      ;;(println "evaluate" o)
      (case (:type o)
        :triangle-list (:vertices o)
        (assert false)))))

(defn vertex-shader
  "input world-space vertices, output projected clip-coord vertices"
  [object-vertices]
  ;;(println "VS" object-vertices)
  (for [o object-vertices]
    (do
      ;;(println "O" o)
      (for [v o]
        (do
          ;;(println "vertex-shader" v)
          (mmul (:projection-matrix @state)
                (:view-matrix @state)
                (:model-matrix @state)
                (column-matrix [(:x v) (:y v) (:z v) 1])))))))

(defn project-viewport
  "input vertices in clip-coords, output processed
  primitives in window-coords"
  [object-vertices]
  (for [vertices object-vertices]
    (do
      (println "pv" vertices)
      (for [v vertices]
        (do
          (println "pvv" v)
          ;; see OpenGL spec 2.11 Coordinate Transformations
          (let [ndc-v (div (submatrix v [[0 3] [0 1]])
                           (submatrix v [[3 1] [0 1]]))
                _ (println "xx" ((ndc-v 1) 0))
                px (:viewport-width @state)
                ox (clojure.core// px 2) ;; FIXME
                window-x (clojure.core/+ (clojure.core/* (clojure.core// px 2)
                                                         ((ndc-v 0) 0))
                                         ox)
                py (:viewport-height @state)
                oy (clojure.core// py 2) ;; FIXME
                window-y (clojure.core/+ (clojure.core/* (clojure.core// py 2)
                                                         ((ndc-v 1) 0))
                                         oy)
                [n f] (:depth-range @state)
                window-z (clojure.core/+ (clojure.core/* (clojure.core// (clojure.core/- f n) 2)
                                                       ((ndc-v 2) 0))
                                         (clojure.core// (clojure.core/+ n f) 2))]
            (column-matrix [window-x window-y window-z])))))))

(defn primitive-clip-cull
  "input projected vertices, output primitives (triangles)"
  [object-vertices]
  (for [vertices object-vertices]
    (do
      (println "pa" vertices)
      (for [p (partition 3 vertices)]
        (do
          ;;(println "primitive-assembly" p)
          p)))))

(defn rasterize
  "input primitives, output rasterized fragments"
  [primitives]
  [nil])

(defn fragment-shader
  "input unshaded fragments, output shaded fragments"
  [fragments]
  [nil])

(defn framebuffer-operations
  "input shaded fragments, write them to the framebuffer"
  [fragments]
  nil)

(defn draw
  "take in an object, decompose it to triangles, pass them through the
  pipeline and output to framebuffer"
  [objects]
  (->> (evaluate objects)
       (vertex-shader)
       (project-viewport)
       (primitive-clip-cull)))
       ;;(rasterize)
       ;;(fragment-shader)
       ;;(framebuffer-operations)))

(defn run []
  ;; while true, with-camera? with-frame?
  (let [objects [{:type     :triangle-list
                  :vertices [{:x 0.0 :y 0.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                             {:x 1.0 :y 0.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                             {:x 1.0 :y 1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}

                             {:x 0.0 :y 0.0 :z 0.5 :r 1.0 :g 0.0 :b 0.0}
                             {:x 0.5 :y 0.0 :z 0.5 :r 0.0 :g 1.0 :b 0.0}
                             {:x 0.5 :y 0.5 :z 0.5 :r 0.0 :g 0.0 :b 1.0}
                             ]}]]
    (draw objects)))

;;(doall (run))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println (doall (run))))
