(ns iris.matrix)
;; a hand-made matrix library only suitable for use in a
;; self-contained project like this.  this is NOT trying to be a real
;; library.  Code was added as-needed.

;; NOTE: Clojure 1.6.0 fixed some issues with type hints on return values.

(defrecord Matrix4x4 [^double m00 ^double m01 ^double m02 ^double m03
                      ^double m10 ^double m11 ^double m12 ^double m13
                      ^double m20 ^double m21 ^double m22 ^double m23
                      ^double m30 ^double m31 ^double m32 ^double m33])
(defrecord Vector2 [^double x ^double y])
(defrecord Vector3 [^double x ^double y ^double z])
(defrecord Vector4 [^double x ^double y ^double z ^double w])

(defn identity-matrix
  "return a 4x4 identity matrix"
  ^Matrix4x4
  []
  (Matrix4x4. 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              0.0 0.0 0.0 1.0))

(defn mmul
  "4x4 matrix multiplication M x N"
  ^Matrix4x4
  [^Matrix4x4 m ^Matrix4x4 n]
  ;; take first matrix by rows.  take second matrix by columns
  (Matrix4x4.
   (+ (* (.m00 m) (.m00 n)) (* (.m01 m) (.m10 n)) (* (.m02 m) (.m20 n)) (* (.m03 m) (.m30 n)))
   (+ (* (.m00 m) (.m01 n)) (* (.m01 m) (.m11 n)) (* (.m02 m) (.m21 n)) (* (.m03 m) (.m31 n)))
   (+ (* (.m00 m) (.m02 n)) (* (.m01 m) (.m12 n)) (* (.m02 m) (.m22 n)) (* (.m03 m) (.m32 n)))
   (+ (* (.m00 m) (.m03 n)) (* (.m01 m) (.m13 n)) (* (.m02 m) (.m23 n)) (* (.m03 m) (.m33 n)))
   (+ (* (.m10 m) (.m00 n)) (* (.m11 m) (.m10 n)) (* (.m12 m) (.m20 n)) (* (.m13 m) (.m30 n)))
   (+ (* (.m10 m) (.m01 n)) (* (.m11 m) (.m11 n)) (* (.m12 m) (.m21 n)) (* (.m13 m) (.m31 n)))
   (+ (* (.m10 m) (.m02 n)) (* (.m11 m) (.m12 n)) (* (.m12 m) (.m22 n)) (* (.m13 m) (.m32 n)))
   (+ (* (.m10 m) (.m03 n)) (* (.m11 m) (.m13 n)) (* (.m12 m) (.m23 n)) (* (.m13 m) (.m33 n)))
   (+ (* (.m20 m) (.m00 n)) (* (.m21 m) (.m10 n)) (* (.m22 m) (.m20 n)) (* (.m23 m) (.m30 n)))
   (+ (* (.m20 m) (.m01 n)) (* (.m21 m) (.m11 n)) (* (.m22 m) (.m21 n)) (* (.m23 m) (.m31 n)))
   (+ (* (.m20 m) (.m02 n)) (* (.m21 m) (.m12 n)) (* (.m22 m) (.m22 n)) (* (.m23 m) (.m32 n)))
   (+ (* (.m20 m) (.m03 n)) (* (.m21 m) (.m13 n)) (* (.m22 m) (.m23 n)) (* (.m23 m) (.m33 n)))
   (+ (* (.m30 m) (.m00 n)) (* (.m31 m) (.m10 n)) (* (.m32 m) (.m20 n)) (* (.m33 m) (.m30 n)))
   (+ (* (.m30 m) (.m01 n)) (* (.m31 m) (.m11 n)) (* (.m32 m) (.m21 n)) (* (.m33 m) (.m31 n)))
   (+ (* (.m30 m) (.m02 n)) (* (.m31 m) (.m12 n)) (* (.m32 m) (.m22 n)) (* (.m33 m) (.m32 n)))
   (+ (* (.m30 m) (.m03 n)) (* (.m31 m) (.m13 n)) (* (.m32 m) (.m23 n)) (* (.m33 m) (.m33 n)))))

;; (mmul (identity-matrix) (identity-matrix))
;; (mmul (identity-matrix) [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])

(defn mvmul
  "4x4 matrix multiplication M x N"
  ^Vector4
  [^Matrix4x4 m ^Vector4 v]
  ;; vector is like a single column in 2nd matrix
  (Vector4. (+ (* (.m00 m) (.x v)) (* (.m01 m) (.y v)) (* (.m02 m) (.z v)) (* (.m03 m) (.w v)))
            (+ (* (.m10 m) (.x v)) (* (.m11 m) (.y v)) (* (.m12 m) (.z v)) (* (.m13 m) (.w v)))
            (+ (* (.m20 m) (.x v)) (* (.m21 m) (.y v)) (* (.m22 m) (.z v)) (* (.m23 m) (.w v)))
            (+ (* (.m30 m) (.x v)) (* (.m31 m) (.y v)) (* (.m32 m) (.z v)) (* (.m33 m) (.w v)))))

;; (mvmul (identity-matrix) [1 2 3 4])

(defn vmuls
  "3d vector times scalar"
  ^Vector3
  [^Vector3 v ^double s]
  (Vector3. (* (.x v) s) (* (.y v) s) (* (.z v) s)))

(defn vmul2s
  "2d vector times scalar"
  ^Vector2
  [^Vector2 v ^double s]
  (Vector2. (* (.x v) s) (* (.y v) s)))

(defn vsub2
  "2d vector subtraction"
  ^Vector2
  [^Vector2 u ^Vector2 v]
  (Vector2. (- (.x u) (.x v)) (- (.y u) (.y v))))

(defn vsub3
  "3d vector subtraction"
  ^Vector3
  [^Vector3 u ^Vector3 v]
  (Vector3. (- (.x u) (.x v)) (- (.y u) (.y v)) (- (.z u) (.z v))))

(defn cross
  "3d vector cross product"
  ^Vector3
  [^Vector3 u ^Vector3 v]
  (Vector3.
   (- (* (.y u) (.z v)) (* (.z u) (.y v)))
   (- (* (.z u) (.x v)) (* (.x u) (.z v)))
   (- (* (.x u) (.y v)) (* (.y u) (.x v)))))

(defn dot3
  "3d vector dot product"
  ^double
  [^Vector3 u ^Vector3 v]
  (+ (* (.x u) (.x v)) (* (.y u) (.y v)) (* (.z u) (.z v))))

(defn div3
  "3d vector divided by scalar"
  ^Vector3
  [^Vector3 u ^double s]
  (Vector3. (/ (.x u) s)
            (/ (.y u) s)
            (/ (.z u) s)))

(defn div ;; div4?
  "4d vector devided by scalar"
  ^Vector4
  [^Vector4 u ^double s]
  (Vector4. (/ (.x u) s)
            (/ (.y u) s)
            (/ (.z u) s)
            (/ (.w u) s)))

(defn vmag ;; cannot add ^double to return ??? FIXME
  "3d vector magnitude"
  ^double
  [^Vector3 v]
  (Math/sqrt (+ (* (.x v) (.x v)) (* (.y v) (.y v)) (* (.z v) (.z v)))))

(defn vnorm
  "3d vector normalization"
  ^Vector3
  [^Vector3 u]
  (let [mag (vmag u)]
    (if (= mag 1.0)
      u
      (if (= mag 0.0)
        (Vector3. 0.0 0.0 0.0)
        (div3 u mag)))))

;; NOTE: OpenGL assumes Column Major Matrices in memory.  The above
;; math is all in Row Major.  So, keep this guide in mind when
;; interpreting OpenGL code that accesses a matrix in memory.
;;
;; [ m01 m05 m09 m13
;;   m02 m06 m10 m14
;;   m03 m07 m11 m15
;;   m04 m08 m12 m16 ]

(defn translate
  "translate 4x4 matrix by 3d vector, returning new matrix"
  ^Matrix4x4
  [^Matrix4x4 m ^Vector3 v]
  (mmul m
        (Matrix4x4.
         1 0 0 (.x v)
         0 1 0 (.y v)
         0 0 1 (.z v)
         0 0 0 1)))

(defn rotate
  "rotate 4x4 matrix by angle radians about a 3d vector, returning new matrix"
  ^Matrix4x4
  [^Matrix4x4 m ^double angle ^Vector3 v]
  (let [c   (Math/cos angle)
        l-c (- 1.0 c)
        s   (Math/sin angle)
        v   (vnorm v)
        x   (.x v)
        y   (.y v)
        z   (.z v)]
    (mmul m
          ;; man page:
          ;;[x^2(1-c)+c xy(1-c)-zs xz(1-c)+ys 0
          ;; yx(1-c)+zs y^2(1-c)+c yz(1-c)-xs 0
          ;; xz(1-c)-ys yz(1-c)+xs z^2(1-c)+c 0
          ;; 0          0          0          1]
          (Matrix4x4.
           (+ (* x x l-c) c)       (- (* x y l-c) (* z s)) (+ (* x z l-c) (* y s)) 0.0
           (+ (* y x l-c) (* z s)) (+ (* y y l-c) c)       (- (* y z l-c) (* x s)) 0.0
           (- (* x z l-c) (* y s)) (+ (* y z l-c) (* x s)) (+ (* z z l-c) c)       0.0
           0.0                     0.0                     0.0                     1.0))))

(defn scale
  "scale 4x4 matrix by 3d vector, returning new matrix"
  ^Matrix4x4
  [^Matrix4x4 m ^Vector3 v]
  (mmul m
        (Matrix4x4.
         (.x v) 0      0      0
         0      (.y v) 0      0
         0      0      (.z v) 0
         0      0      0      1)))

(defn look-at
  "return a 4x4 view matrix from eye 3d vector, to center 3d vector
  with upv 3d vector indicating the 'up' direction."
  ^Matrix4x4
  [^Vector3 eye ^Vector3 center ^Vector3 upv]
  (let [upv     (vnorm upv)
        forward (vnorm (vsub3 center eye))
        side    (vnorm (cross forward upv))
        upv     (cross side forward)] ;; gluLookAt code has this
    (translate (Matrix4x4.
                (.x side)        (.y side)        (.z side)        0.0
                (.x upv)         (.y upv)         (.z upv)         0.0
                (- (.x forward)) (- (.y forward)) (- (.z forward)) 0.0
                0.0              0.0              0.0              1.0)
               (vmuls eye -1.0))))

;; unhinted as these are not perf issues (right?)

(defn ortho
  "return 4x4 orthographic projection matrix"
  [left right bottom top near far]
  (let [tx (- (/ (+ right left) (- right left)))
        ty (- (/ (+ top bottom) (- top bottom)))
        tz (- (/ (+ far near) (- far near)))
        sx (/ 2.0 (- right left))
        sy (/ 2.0 (- top bottom))
        sz (/ 2.0 (- far near))]
    (Matrix4x4.  sx 0  0  tx
                 0  sy 0  ty
                 0  0  sz tz
                 0  0  0  1)))

(defn frustum
  "return 4x4 perspective projection matrix"
  [left right bottom top near far]
  (let [A (/ (+ right left) (- right left))
        B (/ (+ top bottom) (- top bottom))
        C (- (/ (+ far near) (- far near)))
        D (- (/ (* 2 far near) (- far near)))
        E (/ (* 2 far near) (- right left))
        F (/ (* 2 far near) (- top bottom))]
    (Matrix4x4. E 0  A 0
                0 F  B 0
                0 0  C D
                0 0 -1 0)))

(defn perspective
  "return 4x4 perspective projection matrix"
  [fov-y aspect near far]
  (let [f (/ (Math/cos (/ fov-y 2.0)) (Math/sin (/ fov-y 2.0))) ; cotangent
        C (- (/ (+ far near) (- far near)))
        D (- (/ (* 2 far near) (- far near)))
        F (/ f aspect)]
    (Matrix4x4. F 0  0 0
                0 f  0 0
                0 0  C D
                0 0 -1 0)))
