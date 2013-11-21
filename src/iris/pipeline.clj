(ns iris.pipeline
  (:require [iris.matrix :as mat]
            [iris.geometry :as g]))

;;(set! *warn-on-reflection* true)

(defn evaluate
  "given an object, evaluate it to decompose it into triangles"
  [state objects]
  (for [o objects]
    (g/evaluate-object o)))

;; (partial vertex-shader your-vs) ??
(defn vertex-shader
  "input world-space vertices, output projected clip-coord vertices"
  [state object-vertices]
  ;;(println "VS" object-vertices)
  (for [vertices object-vertices]
    (do
      ;;(println "O" o)
      (for [v vertices]
        (do
          ;;(println "vertex-shader" v)
          (into v {:clip
                   ;; v-clip = MVP * v
                   (mat/mvmul
                    ;; MVP = P * MV
                    (mat/mmul (:projection-matrix state)
                              ;; MV = V * M
                              (mat/mmul (:view-matrix state)
                                        (:model-matrix state)))
                    [(:x v) (:y v) (:z v) 1])
                   })
          )))))

(defn project-viewport
  "input vertices in clip-coords, output processed primitives in
  window-coords"
  [state object-vertices]
  (for [vertices object-vertices]
    (do
      (for [v vertices]
        (do
          ;; see OpenGL spec 2.11 Coordinate Transformations
          (let [clip-v (:clip v)
                ;; ndc = x/w y/w z/w 1/w
                ndc-v (mat/div
                       (concat (take 3 clip-v) [1])
                       (nth clip-v 3))
                [vox voy px py] (:viewport state)
                ox       (+ vox (/ px 2))
                window-x (+ (* (/ px 2) (ndc-v 0)) ox)
                oy       (+ voy (/ py 2))
                window-y (+ (* (/ py 2) (ndc-v 1)) oy)
                [n f]    (:depth-range state)
                window-z (+ (* (/ (- f n) 2) (ndc-v 2)) (/ (+ n f) 2))
                window-w (ndc-v 3)
                ]
            (into v {:window [window-x window-y window-z window-w]})
            ))))))

(defn primitive-clip-cull
  "input projected vertices, output primitives (triangles)"
  [state object-vertices]
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
  (let [p1 (concat P1 [0])
        p2 (concat P2 [0])
        a  (concat A [0])
        b  (concat B [0])
        cp1 (mat/cross (mat/vsub3 b a) (mat/vsub3 p1 a))
        ;;_ (println cp1)
        cp2 (mat/cross (mat/vsub3 b a) (mat/vsub3 p2 a))
        ;;_ (println cp2)
        dp (mat/dot3 cp1 cp2)]
        ;;_ (println dp)]
    (>= dp 0.0)))

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

(defn in-viewport?
  [state x y]
  (and (> x ((:viewport state) 0))
       (< x (+ ((:viewport state) 0) ((:viewport state) 2)))
       (> y ((:viewport state) 1))
       (< y (+ ((:viewport state) 1) ((:viewport state) 3)))))

(defn in-fb?
  [state x y]
  (and (> x ((:fbport state) 0))
       (< x (+ ((:fbport state) 0) ((:fbport state) 2)))
       (> y ((:fbport state) 1))
       (< y (+ ((:fbport state) 1) ((:fbport state) 3)))))

(defn rasterize
  "input primitives, output rasterized fragments"
  [state object-primitives]
  (for [primitives object-primitives]
    (do
      ;;(println "prims" primitives)
      (for [prim primitives]
        (do
          ;;(println "prim" prim)
          {:vertices prim
           :pixels (filter (fn [[x y]]
                             (and (in-viewport? state x y)
                                  (in-fb? state x y)))
                           (rasterize-triangle (:window (nth prim 0))
                                               (:window (nth prim 1))
                                               (:window (nth prim 2))))})))))
;;(println (run))

(defn triangle-area
  [a b c]
  (let [ab (concat (mat/vsub2 b a) [0])
        ac (concat (mat/vsub2 c a) [0])
        ;; wow, the following abs annotation helped significantly
        mag (Math/abs ^double (nth (mat/cross ab ac) 2))] ;; we know mag is all in Z comp
  (* 0.5 mag)))
;; (triangle-area [0 0] [10 0] [10 10])

(defn interpolate
  "interpolate barycentric coords according to formula 3.6 in OpenGL Spec 1.5"
  [attr prim aow bow cow]
  (let [fa (attr (nth prim 0))
        fb (attr (nth prim 1))
        fc (attr (nth prim 2))
        ]
    ;;(println "intrp" fa fb fc aow bow cow)
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
  [state objects-prims-pixels]
  ;;(println "PS" object-prim-pixels)
  (for [prims-pixels objects-prims-pixels]
    (for [prim-pixels prims-pixels]
      (let [prim (:vertices prim-pixels)
            pixels (:pixels prim-pixels)]
        ;;(println "prim-pixels" prim )
        {:vertices prim
         :pixels (for [p pixels]
                   (let [x (first p)
                         y (second p)]
                     ;;(println "pixel-shader" x y)
                     (into {:x x :y y} (shade-pixel prim x y))))}
        ))))

(defn framebuffer-operations*
  [state framebuffer object-prim-pixels]
  (persistent!
   (reduce
    ;; reduce into a transient copy of the framebuffer
    (fn [fb [i src-pixel]]
      (let [dest-pixel (nth fb i)]
        (if (< (:z src-pixel) (:z dest-pixel))
          (assoc! fb i src-pixel)
          fb)))
    (transient (:data framebuffer))
    ;; from a list of all the pixels to update
    (for [src-pixel (flatten (map #(map :pixels %) object-prim-pixels))]
      (let [w (:width framebuffer)
            h (:height framebuffer)
            x (Math/floor (- (:x src-pixel) ((:fbport state) 0)))
            y (Math/floor (- (:y src-pixel) ((:fbport state) 1)))
            ;;_ (println "fb" x y)
            i (int (+ (* y w) x))]
        [i src-pixel])))))

(defn framebuffer-operations
  "input shaded fragments, write them to the framebuffer"
  [state framebuffer object-prim-pixels]
  (into framebuffer {:data (framebuffer-operations* state
                                                    framebuffer
                                                    object-prim-pixels)}))

(defn resolve-framebuffers
  "combine future-sources into dest framebuffer.  assumes they stack in y"
  [dest future-sources]
  (assoc dest :data (apply vector
                           (apply concat
                                  (map #(:data @%) future-sources)))))

(defn debug-stage
  [lbl x]
  (binding [*out* *err*]
    (println "======================================================================")
    (println lbl)
    (println x)
    (println "======================================================================"))
  x)

(defn render-framebuffer
  "take in an object, decompose it to triangles, pass them through the
  pipeline and output to framebuffer"
  [state framebuffer objects]
  (->> (evaluate state objects)
       ;;(debug-stage "evaluate output")
       (vertex-shader state)
       ;;(debug-stage "vertex-shader output")
       (project-viewport state)
       ;;(debug-stage "project-viewport output")
       (primitive-clip-cull state)
       ;;(debug-stage "clip-cull output")
       (rasterize state)
       ;;(debug-stage "rasterize output")
       (pixel-shader state)
       ;;(debug-stage "pixel-shader output")
       (framebuffer-operations state framebuffer)))

(defn parallel-render-framebuffer ;; ??? multimethod?
  [n state framebuffer objects]
  (->>
   (let [w (:width framebuffer)
         h (:height framebuffer)
         hon (/ h n)
         _ (assert (= (rem h n) 0))
         [fbx fby fbw fbh] (:fbport state)]
     (for [cur-fby (range 0 h hon)]
       (future
         (render-framebuffer
                (assoc state
                  :fbport [fbx cur-fby fbw hon])
                (assoc framebuffer
                  :y      cur-fby
                  :height hon
                  ;; allocate the data for sub-framebuffers
                  :data   (vec (repeat (* w hon)
                                       {:r 0 :g 0 :b 0 :z 1000}))
                  )
                objects))))
   (resolve-framebuffers framebuffer)))
