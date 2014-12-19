(ns iris.pipeline
  (:require [clojure.pprint :as pp]
            [iris.matrix :as mat]
            [iris.geometry :as g])
  (:import  [iris.matrix Matrix4x4 Vector2 Vector3 Vector4]))

;;(set! *warn-on-reflection* true)
(defn v2->v3 [v2] (Vector3. (.x v2) (.y v2) 0.0))
(defn v3->v2 [v3] (Vector2. (.x v3) (.y v3)))
(defn v4->v3 [v4] (Vector3. (.x v4) (.y v4) (.y v4)))

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
                  (Vector4. (:nx v) (:ny v) (:nz v) 0))
          clip-n (v4->v3 clip-n)
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
                (Vector4. (:x v) (:y v) (:z v) 1))
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
  (let [p1  (v2->v3 P1)
        p2  (v2->v3 P2)
        a   (v2->v3 A)
        b   (v2->v3 B)
        cp1 (mat/cross (mat/vsub3 b a) (mat/vsub3 p1 a))
        cp2 (mat/cross (mat/vsub3 b a) (mat/vsub3 p2 a))
        dp  (mat/dot3 cp1 cp2)]
    (>= dp 0.0)))

(defn pt-inside-tri?
  "is pt inside the triangle VA VB VC?"
  [va vb vc pt]
  (let [A (v3->v2 va)
        B (v3->v2 vb)
        C (v3->v2 vc)]
    (and (same-side? pt A B C)
         (same-side? pt B A C)
         (same-side? pt C A B))))

(defn inside-port?
  "ix x,y inside a viewport with they key view-key?"
  [state view-key x y]
  (and (>= x (.x (view-key state)))
       (< x (+ (.x (view-key state)) (.z (view-key state))))
       (>= y (.y (view-key state)))
       (< y (+ (.y (view-key state)) (.w (view-key state))))))

(defn in-viewport?
  "is x,y inside the viewport?"
  [state x y]
  (inside-port? state :viewport x y))

(defn in-fb?
  "is x,y inside the framebuffer port that your thread owns?"
  [state x y]
  (inside-port? state :fbport x y))

(defn rasterize-triangle
  "given 3 2d points, output the xy pairs describing the points inside."
  [state va vb vc]
  (let [x-min (Math/floor ^double (min (.x va) (.x vb) (.x vc)))
        x-max (Math/ceil  ^double (max (.x va) (.x vb) (.x vc)))
        y-min (Math/floor ^double (min (.y va) (.y vb) (.y vc)))
        y-max (Math/ceil  ^double (max (.y va) (.y vb) (.y vc)))]
    ;; it turned out to be very important for performance to make sure
    ;; we only rasterized pixels inside our thread's framebuffer.
    (filter (fn [pt] (and (in-fb? state (.x pt) (.y pt))
                         (in-viewport? state (.x pt) (.y pt))
                         (pt-inside-tri? va vb vc pt)))
            ;; Be careful: (range 1.2 3.7) ==> (1.2 2.2 3.2).  Use floor/ceil
            (for [x (range x-min x-max)
                  y (range y-min y-max)]
              ;; pixels are centered in the middle
              (Vector2. (+ x 0.5) (+ y 0.5))))))

;; ======================================================================
;; pixel shader helper routines

(defn triangle-area
  "find the area of the triangle given 3 2d points"
  [a b c]
  (let [ab (v2->v3 (mat/vsub2 b a))
        ac (v2->v3 (mat/vsub2 c a))
        ;; we know mag is all in Z component when crossing xy0 vectors
        ;; wow, the following abs annotation helped perf significantly
        mag (Math/abs ^double (.z (mat/cross ab ac)))]
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
  [prim pt]
  (let [pa (v3->v2 (:window (nth prim 0)))
        pb (v3->v2 (:window (nth prim 1)))
        pc (v3->v2 (:window (nth prim 2)))
        oowa (.w (:window (nth prim 0)))
        oowb (.w (:window (nth prim 1)))
        oowc (.w (:window (nth prim 2)))
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
        z-prim (map #(hash-map :z (.z (:window (nth prim %)))) (range 3))
        z (interpolate :z z-prim aow bow cow)]
    {:x (.x pt) :y (.y pt) :z z :r r :g g :b b}))

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
            x (Math/floor ^double (- (:x src-pixel) (.x (:fbport state))))
            y (Math/floor ^double (- (:y src-pixel) (.y (:fbport state))))
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
    (pp/pprint x)
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
            ndc-v    (mat/div
                      (Vector4. (.x clip-v) (.y clip-v) (.z clip-v) 1.0)
                      (.w clip-v))
            vox      (.x (:viewport state))
            voy      (.y (:viewport state))
            px       (.z (:viewport state))
            py       (.w (:viewport state))
            ox       (+ vox (/ px 2))
            window-x (+ (* (/ px 2) (.x ndc-v)) ox)
            oy       (+ voy (/ py 2))
            window-y (+ (* (/ py 2) (.y ndc-v)) oy)
            n        (.x  (:depth-range state))
            f        (.y  (:depth-range state))
            window-z (+ (* (/ (- f n) 2) (.z ndc-v)) (/ (+ n f) 2))
            window-w (.w ndc-v)
            ]
        (into v {:window (Vector4. window-x window-y window-z window-w)})))))

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
                                   (:window (nth prim 0))
                                   (:window (nth prim 1))
                                   (:window (nth prim 2)))})))

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
       ;;(debug-stage "a")
       (framebuffer-operations state framebuffer)))

(defn parallel-render-framebuffer
  "Allow for 'embarassing parallelism' and the speedup that can come
  with this.  Divide up rendering into n horizontal swaths.  n must
  divide into the framebuffer height evenly"
  [n state framebuffer objects]
   (resolve-framebuffers
    framebuffer
    (let [w   (:width framebuffer)
          h   (:height framebuffer)
          hon (int (/ h n))
          _   (assert (zero? (rem h n)))
          fbx (.x (:fbport state))
          fby (.y (:fbport state))
          fbw (.z (:fbport state))
          fbh (.w (:fbport state))]
      (for [cur-fby (range 0 h hon)]
        (future
          (render-framebuffer
           (assoc state
             :fbport (Vector4. fbx cur-fby fbw hon))
           (assoc framebuffer
             :y      cur-fby
             :height hon
             ;; allocate the data for sub-framebuffers
             :data   (vec (repeat (* w hon)
                                  {:r 0 :g 0 :b 0 :z 1000}))
             )
           objects))))))
