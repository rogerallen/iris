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
                                               {:r 0 :g 0 :b 0 :z 1})}
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
                   (mat/get-column
                    (mat/mmul (:projection-matrix @state)
                              (:view-matrix @state)
                              (:model-matrix @state)
                              (mat/column-matrix [(:x v) (:y v) (:z v) 1]))
                    0)})
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
                ;; ndc = x/w y/w z/w 1/w
                ndc-v (mat/div
                       (mat/join (take 3 clip-v) [1])
                       (nth clip-v 3))
                [vox voy px py] (:viewport @state)
                ox       (+ vox (/ px 2))
                window-x (+ (* (/ px 2) (ndc-v 0)) ox)
                oy       (+ voy (/ py 2))
                window-y (+ (* (/ py 2) (ndc-v 1)) oy)
                [n f]    (:depth-range @state)
                window-z (+ (* (/ (- f n) 2) (ndc-v 2)) (/ (+ n f) 2))
                window-w (ndc-v 3)
                ]
            (into v {:window [window-x window-y window-z window-w]})
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
  (let [A (take 2 va)
        B (take 2 vb)
        C (take 2 vc)]
    (and (same-side pt A B C)
         (same-side pt B A C)
         (same-side pt C A B))))

(defn rasterize-triangle
  [va vb vc]
  (let [x-min (min (va 0) (vb 0) (vc 0))
        x-max (max (va 0) (vb 0) (vc 0))
        y-min (min (va 1) (vb 1) (vc 1))
        y-max (max (va 1) (vb 1) (vc 1))]
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
      {:vertices prim
       :pixels (rasterize-triangle (:window (nth prim 0))
                                   (:window (nth prim 1))
                                   (:window (nth prim 2)))})))
;;(println (run))

(defn triangle-area
  [a b c]
  (let [ab (mat/join (mat/sub b a) [0])
        ac (mat/join (mat/sub c a) [0])
        aa (mat/cross ab ac)
        mag (Math/abs (nth aa 2))]
  (* 0.5 mag)))
;; (triangle-area [0 0] [10 0] [10 10])

(defn interpolate
  "interpolate barycentric coords according to formula 3.6 in OpenGL Spec 1.5"
  [attr prim aow bow cow]
  (let [fa (attr (nth prim 0))
        fb (attr (nth prim 1))
        fc (attr (nth prim 2))
        ]
    (/ (+ (* fa aow) (* fb bow) (* fc cow))
       (+ (* 1 aow) (* 1 bow) (* 1 cow))))) ;; FIXME for q

(defn shade-pixel
  [prim x y]
  (let [pt [x y]
        pa (take 2 (:window (nth prim 0)))
        pb (take 2 (:window (nth prim 1)))
        pc (take 2 (:window (nth prim 2)))
        oowa (nth (:window (nth prim 0)) 3)
        oowb (nth (:window (nth prim 1)) 3)
        oowc (nth (:window (nth prim 2)) 3)
        Aabc (triangle-area pa pb pc)
        Apbc (triangle-area pt pb pc)
        Apac (triangle-area pt pa pc)
        Apab (triangle-area pt pa pb)
        a (/ Apbc Aabc)
        b (/ Apac Aabc)
        c (/ Apab Aabc) ;; 1-a-b ?
        aow (* a oowa)
        bow (* b oowb)
        cow (* c oowc)
        r (interpolate :r prim aow bow cow)
        g (interpolate :g prim aow bow cow)
        b (interpolate :b prim aow bow cow)
        z-prim (map #(hash-map :z (nth (:window (nth prim %)) 2)) (range 3))
        z (interpolate :z z-prim aow bow cow)
        ]
    {:x x :y y :z z :r r :g g :b b}))


(defn pixel-shader
  "input unshaded pixels, output shaded pixels"
  [object-prim-pixels]
  ;;(println "PS" object-prim-pixels)
  (for [prim-pixels object-prim-pixels]
    (let [prim (:vertices prim-pixels)
          pixels (:pixels prim-pixels)]
      ;;(println "prim-pixels" prim pixels)
      {:vertices prim
       :pixels (for [p pixels]
                  (let [x (first p)
                        y (second p)]
                    ;;(println "pixel-shader" x y)
                    (into {:x x :y y} (shade-pixel prim x y))))}
      )))

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
       (rasterize)
       (pixel-shader)))
       ;;(framebuffer-operations)))

;;(println (run))

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

;;(println (run))

(defn -main
  [& args]
  (println "Iris is Drawing...")
  (pp/pprint (doall (run))))
