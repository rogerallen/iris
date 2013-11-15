(ns iris.core
  (:require [clojure.core.matrix :as mat])
  (:require [clojure.pprint :as pp])
  (:gen-class))

(defonce state (atom {:framebuffer-width 32
                      :framebuffer-height 24
                      :viewport [0 0 32 24]
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
                      }))

(defonce framebuffer (atom (vec {:width 32
                                 :height 24
                                 :data (repeat (* 32 24)
                                               {:r 0 :g 0 :b 0})}
                                )))

(defn evaluate
  "given an object, evaluate it to decompose it into triangles"
  [objects]
  (for [o objects]
    (do
      ;;(println "evaluate" o)
      (case (:type o)
        :triangle-list (:vertices o)
        (assert false)))))

;; (partial vertex-shader your-vs) ??
(defn vertex-shader
  "input world-space vertices, output projected clip-coord vertices"
  [object-vertices]
  ;;(println "VS" object-vertices)
  (for [vertices object-vertices]
    (do
      ;;(println "O" o)
      (for [v vertices]
        (do
          ;;(println "vertex-shader" v)
          (into v {:clip
                   (mat/mmul (:projection-matrix @state)
                             (:view-matrix @state)
                             (:model-matrix @state)
                             (mat/column-matrix [(:x v) (:y v) (:z v) 1]))})
          )))))

(defn project-viewport
  "input vertices in clip-coords, output processed primitives in
  window-coords"
  [object-vertices]
  (for [vertices object-vertices]
    (do
      (for [v vertices]
        (do
          ;; see OpenGL spec 2.11 Coordinate Transformations
          (let [clip-v (:clip v)
                ndc-v (mat/div (mat/submatrix clip-v [[0 3] [0 1]])
                               (mat/submatrix clip-v [[3 1] [0 1]]))
                [vox voy px py] (:viewport @state)
                ox       (+ vox (/ px 2))
                window-x (+ (* (/ px 2) ((ndc-v 0) 0)) ox)
                oy       (+ voy (/ py 2))
                window-y (+ (* (/ py 2) ((ndc-v 1) 0)) oy)
                [n f]    (:depth-range @state)
                window-z (+ (* (/ (- f n) 2) ((ndc-v 2) 0)) (/ (+ n f) 2))]
            (into v {:window
                     (mat/column-matrix [window-x window-y window-z])})
            ))))))

(defn primitive-clip-cull
  "input projected vertices, output primitives (triangles)"
  [object-vertices]
  (for [vertices object-vertices]
    (do
      ;;(println "pa" vertices)
      (for [p (partition 3 vertices)]
        (do
          ;;(println "primitive-assembly" p)
          ;; clipless rasterizer :^) FIXME
          p)))))

;; http://www.blackpawn.com/texts/pointinpoly/
(defn same-side
  [P1 P2 A B]
  (let [p1 (mat/join P1 [0])
        p2 (mat/join P2 [0])
        a  (mat/join A [0])
        b  (mat/join B [0])
        cp1 (mat/cross (mat/sub b a) (mat/sub p1 a))
        ;;_ (println cp1)
        cp2 (mat/cross (mat/sub b a) (mat/sub p2 a))
        ;;_ (println cp2)
        dp (mat/dot cp1 cp2)]
        ;;_ (println dp)]
    (>= dp 0)))

(defn pt-inside-tri?
  [va vb vc pt]
  (let [A (mat/get-column (mat/submatrix va [[0 2] [0 1]]) 0)
        B (mat/get-column (mat/submatrix vb [[0 2] [0 1]]) 0)
        C (mat/get-column (mat/submatrix vc [[0 2] [0 1]]) 0)]
    (and (same-side pt A B C)
         (same-side pt B A C)
         (same-side pt C A B))))

(defn rasterize-triangle
  [va vb vc]
  (let [x-min (min ((va 0) 0) ((vb 0) 0) ((vc 0) 0))
        x-max (max ((va 0) 0) ((vb 0) 0) ((vc 0) 0))
        y-min (min ((va 1) 0) ((vb 1) 0) ((vc 1) 0))
        y-max (max ((va 1) 0) ((vb 1) 0) ((vc 1) 0))]
    (filter #(pt-inside-tri? va vb vc %)
            (for [x (range x-min x-max)
                  y (range y-min y-max)]
              ;; pixels are centered in the middle
              [(+ x 0.5) (+ y 0.5)]))))

(defn rasterize
  "input primitives, output rasterized fragments"
  [primitives]
  ;;(println "ra prims" primitives)
  (for [prim (first primitives)] ;; FIXME
    (do
      ;;(println "ra prim" prim)
      (into {} {:vertices prim
                :pixels (rasterize-triangle (:window (nth prim 0))
                                            (:window (nth prim 1))
                                            (:window (nth prim 2)))}))))

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
       (primitive-clip-cull)
       (rasterize)))
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
  (pp/pprint (doall (run))))
