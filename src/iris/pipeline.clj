(ns iris.pipeline
  (:require [iris.matrix :as mat]
            [iris.geometry :as g]))

;;(set! *warn-on-reflection* true)

;; ======================================================================
;; vertex shader helper routines
(defn vertex-light
  [state v]
  (if (contains? state :light-vector)
    ;; a little ambient + diffuse shader
    (let [clip-n (mat/mvmul
                  (mat/mmul (:projection-matrix state)
                            (mat/mmul (:view-matrix state)
                                      (:model-matrix state)))
                      [(:nx v) (:ny v) (:nz v) 0])
          clip-n (mat/vnorm clip-n)
          n-dot-l (max 0 (mat/dot3 clip-n (:light-vector state)))]
      {:r (min 1.0 (+ (* 0.2 (:r v)) (* n-dot-l (:r v))))
       :g (min 1.0 (+ (* 0.2 (:g v)) (* n-dot-l (:g v))))
       :b (min 1.0 (+ (* 0.2 (:b v)) (* n-dot-l (:b v))))}
      )
    ;; else
    {:r (:r v) :g (:g v) :b (:b v)}))

(defn shade-vertex
  [state v]
  (let [clip-v (mat/mvmul ;; v-clip = MVP * v
                ;; MVP = P * MV
                (mat/mmul (:projection-matrix state)
                          ;; MV = V * M
                          (mat/mmul (:view-matrix state)
                                    (:model-matrix state)))
                [(:x v) (:y v) (:z v) 1])
        rgb-v (vertex-light state v)
        ]
    (into v {:clip clip-v   ;; required clip-coord vert
             :r (:r rgb-v)  ;; other attrs required by pixel shader
             :g (:g rgb-v)
             :b (:b rgb-v)
             })))

;; ======================================================================
;; rasterization helper routines

;; http://www.blackpawn.com/texts/pointinpoly/
(defn same-side?
  "are P1 and P2 both on the same side of the line through AB?"
  [P1 P2 A B]
  (let [;; 2d cross products put all the data in .z, optimize for that
        cp1 (mat/cross2s (mat/vsub2 B A) (mat/vsub2 P1 A))
        cp2 (mat/cross2s (mat/vsub2 B A) (mat/vsub2 P2 A))
        ;; a dot product does more work than necessary
        ;;dp  (mat/dot3 cp1 cp2)
        ;; optimized to
        dp (* cp1 cp2)]
    (>= dp 0.0)))

(defn pt-inside-tri?
  "is pt inside the triangle VA VB VC?"
  [A B C pt]
  (and (same-side? pt A B C)
       (same-side? pt B A C)
       (same-side? pt C A B)))

(defn in-viewport?
  "is x,y inside the viewport?"
  [state x y]
  (and (>= x ((:viewport state) 0))
       (< x (+ ((:viewport state) 0) ((:viewport state) 2)))
       (>= y ((:viewport state) 1))
       (< y (+ ((:viewport state) 1) ((:viewport state) 3)))))

(defn in-fb?
  "is x,y inside the framebuffer port that your thread owns?"
  [state x y]
  (and (>= x ((:fbport state) 0))
       (< x (+ ((:fbport state) 0) ((:fbport state) 2)))
       (>= y ((:fbport state) 1))
       (< y (+ ((:fbport state) 1) ((:fbport state) 3)))))

(defn rasterize-triangle
  "given 3 2d points, output the xy pairs describing the points inside."
  [state va vb vc]
  (let [x-min (Math/floor (min (va 0) (vb 0) (vc 0)))
        x-max (Math/ceil (max (va 0) (vb 0) (vc 0)))
        y-min (Math/floor (min (va 1) (vb 1) (vc 1)))
        y-max (Math/ceil (max (va 1) (vb 1) (vc 1)))]
    ;; it turned out to be very important for performance to make sure
    ;; we only rasterized pixels inside our thread's framebuffer.
    (filter (fn [[x y]] (and (in-fb? state x y)
                            (in-viewport? state x y)
                            (pt-inside-tri? va vb vc [x y])))
            ;; Be careful: (range 1.2 3.7) ==> (1.2 2.2 3.2).  Use floor/ceil
            (for [x (range x-min x-max)
                  y (range y-min y-max)]
              ;; pixels are centered in the middle
              [(+ x 0.5) (+ y 0.5)]))))

;; ======================================================================
;; pixel shader helper routines

(defn triangle-area
  "find the area of the triangle given 3 2d points"
  [a b c]
  (let [ab (mat/vsub2 b a)
        ac (mat/vsub2 c a)
        ;; we know mag is all in Z component when crossing xy0 vectors
        ;; wow, the following abs annotation helped perf significantly
        mag (Math/abs ^double (mat/cross2s ab ac))]
  (* 0.5 mag)))

(defn interpolate
  "interpolate barycentric coords according to formula 3.6 in OpenGL Spec 1.5"
  [attr prim aow bow cow]
  (let [fa (attr (nth prim 0))
        fb (attr (nth prim 1))
        fc (attr (nth prim 2))]
    (/ (+ (* fa aow) (* fb bow) (* fc cow))
       (+ (* 1 aow) (* 1 bow) (* 1 cow))))) ;; FIXME for q

(defn shade-pixel
  "shade a single pixel and derive the :r :g :b color and :z depth. :x
   and :y are also passed through"
  [prim [x y]]
  (let [pt [x y]
        pa (subvec (:window (nth prim 0)) 0 2)
        pb (subvec (:window (nth prim 1)) 0 2)
        pc (subvec (:window (nth prim 2)) 0 2)
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

;; ======================================================================
;; framebuffer operation helper routines

(defn framebuffer-operations*
  "input shaded fragments, write them into the framebuffer.  A
   transient structure is used here as a performance optimization"
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
            i (int (+ (* y w) x))]
        [i src-pixel])))))

(defn resolve-framebuffers
  "combine future-sources into dest framebuffer.  assumes they stack in y"
  [dest future-sources]
  (assoc dest :data (apply vector (mapcat #(:data @%) future-sources))))

;; ======================================================================
(defn debug-stage
  [lbl x]
  (binding [*out* *err*]
    (println "============================================================")
    (println lbl)
    (println x)
    (println "============================================================"))
  x)

;; ======================================================================
;; The graphics pipeline

(defn evaluate
  "for each input object, evaluate it to decompose it into a sequence
   of world-space vertices"
  [state objects]
  (for [o objects]
    (g/evaluate-object o)))

;; FIXME -- allow user to update vertex-shader instead of hard-coding
(defn shade-vertices
  "for each input object, containing a sequence of world-space
   vertices, output shaded clip-coord vertices as a map with :clip
   holding the clip-coords and other attributes required by the pixel
   shader."
  [state object-vertices]
  (for [vertices object-vertices]
    (for [v vertices]
      (shade-vertex state v))))

(defn project-viewport
  "for each input object and input clip-coord vertices, output
   window-coord vertices in :window"
  [state object-vertices]
  (for [vertices object-vertices]
    (for [v vertices]
      ;; see OpenGL spec 2.11 Coordinate Transformations
      (let [clip-v (:clip v)
            ;; Normalized Device Coords = x/w y/w z/w 1/w
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
        (into v {:window [window-x window-y window-z window-w]})))))

(defn primitive-clip-cull
  "for each object and window-space vertices, gather them into
   3-vertex triangles, clip them (FIXME), cull them (FIXME) and output
   as 3-vertex sequences"
  [state object-vertices]
  (for [vertices object-vertices]
    (for [p (partition 3 vertices)]
      ;; FIXME -- add clipping when necessary
      ;; FIXME -- add backface culling
      p)))

(defn rasterize
  "for each object and primitive, output a rasterized-primitive map of
   the input :prim (for use in barycentric pixel-shader attribute
   calculations) and all the [x y] pixel-center pairs of :pixels that
   result when the triangle is rasterized."
  [state object-primitives]
  (for [primitives object-primitives]
    (for [prim primitives]
      {:prim prim
       :pixels (rasterize-triangle state
                                   (subvec (:window (nth prim 0)) 0 2)
                                   (subvec (:window (nth prim 1)) 0 2)
                                   (subvec (:window (nth prim 2)) 0 2))})))

;; FIXME -- allow user to specify a pixel shader
(defn shade-pixels
  "for each object and primitive containing unshaded pixel centers,
   output shaded pixels containing :x :y :z and :r :g :b"
  [state objects-prims-pixels]
  (for [prims-pixels objects-prims-pixels]
    (for [prim-pixels prims-pixels]
      (into prim-pixels {:pixels (map #(shade-pixel (:prim prim-pixels) %)
                                      (:pixels prim-pixels))}))))

(defn framebuffer-operations
  "input shaded fragments, write them to the framebuffer"
  [state framebuffer object-prim-pixels]
  (into framebuffer
        {:data (framebuffer-operations* state
                                        framebuffer
                                        object-prim-pixels)}))

;; ======================================================================

(defn render-framebuffer
  "Take in a list of objects, evaluate each objects, turning it into triangles,
  pass the triangles through the graphics pipeline and output the
  colored pixels to framebuffer.  Pipeline is similar to the early
  OpenGL pipeline."
  [state framebuffer objects]
  (->> (evaluate state objects)
       (shade-vertices state)
       (project-viewport state)
       (primitive-clip-cull state)
       (rasterize state)
       (shade-pixels state)
       (framebuffer-operations state framebuffer)))

(defn parallel-render-framebuffer
  "Allow for 'embarassing parallelism' and the speedup that can come
  with this.  Divide up rendering into n horizontal swaths.  n must
  divide into the framebuffer height evenly"
  [n state framebuffer objects]
   (resolve-framebuffers
    framebuffer
    (let [w (:width framebuffer)
          h (:height framebuffer)
          hon (int (/ h n))
          _ (assert (zero? (rem h n)))
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
           objects))))))
