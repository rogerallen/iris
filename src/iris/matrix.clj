(ns iris.matrix)

(defn identity-matrix []
  [1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0])

(defn mmul
  "4x4 matrix multiplication M x N"
  ^doubles [^doubles [m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33]
            ^doubles [n00 n01 n02 n03
                      n10 n11 n12 n13
                      n20 n21 n22 n23
                      n30 n31 n32 n33]]
  ;; take first matrix by rows.  take second matrix by columns
  [(+ (* m00 n00) (* m01 n10) (* m02 n20) (* m03 n30))
   (+ (* m00 n01) (* m01 n11) (* m02 n21) (* m03 n31))
   (+ (* m00 n02) (* m01 n12) (* m02 n22) (* m03 n32))
   (+ (* m00 n03) (* m01 n13) (* m02 n23) (* m03 n33))
   (+ (* m10 n00) (* m11 n10) (* m12 n20) (* m13 n30))
   (+ (* m10 n01) (* m11 n11) (* m12 n21) (* m13 n31))
   (+ (* m10 n02) (* m11 n12) (* m12 n22) (* m13 n32))
   (+ (* m10 n03) (* m11 n13) (* m12 n23) (* m13 n33))
   (+ (* m20 n00) (* m21 n10) (* m22 n20) (* m23 n30))
   (+ (* m20 n01) (* m21 n11) (* m22 n21) (* m23 n31))
   (+ (* m20 n02) (* m21 n12) (* m22 n22) (* m23 n32))
   (+ (* m20 n03) (* m21 n13) (* m22 n23) (* m23 n33))
   (+ (* m30 n00) (* m31 n10) (* m32 n20) (* m33 n30))
   (+ (* m30 n01) (* m31 n11) (* m32 n21) (* m33 n31))
   (+ (* m30 n02) (* m31 n12) (* m32 n22) (* m33 n32))
   (+ (* m30 n03) (* m31 n13) (* m32 n23) (* m33 n33))])

;; (mmul (identity-matrix) (identity-matrix))
;; (mmul (identity-matrix) [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])

(defn mvmul
  "4x4 matrix multiplication M x N"
  ^doubles [^doubles [m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33]
            ^doubles [v0 v1 v2 v3]]
  ;; vector is like a single column in 2nd matrix
  [(+ (* m00 v0) (* m01 v1) (* m02 v2) (* m03 v3))
   (+ (* m10 v0) (* m11 v1) (* m12 v2) (* m13 v3))
   (+ (* m20 v0) (* m21 v1) (* m22 v2) (* m23 v3))
   (+ (* m30 v0) (* m31 v1) (* m32 v2) (* m33 v3))])

;; (mvmul (identity-matrix) [1 2 3 4])

(defn vmuls
  ^doubles [^doubles [x y z] ^double s]
  [(* x s) (* y s) (* z s)])

(defn vsub2
  "2d vector subtraction"
  ^doubles [^doubles [u0 u1]
            ^doubles [v0 v1]]
  [(- u0 v0) (- u1 v1)])

(defn vsub3
  "3d vector subtraction"
  ^doubles [^doubles [u0 u1 u2]
            ^doubles [v0 v1 v2]]
  [(- u0 v0) (- u1 v1) (- u2 v2)])

(defn cross
  "3d vector cross product"
  ^doubles [^doubles [u0 u1 u2]
            ^doubles [v0 v1 v2]]
  [(- (* u1 v2) (* u2 v1))
   (- (* u2 v0) (* u0 v2))
   (- (* u0 v1) (* u1 v0))])

;; ??? if I put ^doubles on the return, this gives a error
;; cannot be cast to clojure.lang.IFn
(defn dot3
  [^doubles [u0 u1 u2]
   ^doubles [v0 v1 v2]]
  (+ (* u0 v0) (* u1 v1) (* u2 v2)))

(defn div3
  ^doubles [^doubles [u0 u1 u2]
            ^double v]
  [(/ u0 v)
   (/ u1 v)
   (/ u2 v)])

(defn div ;; FIXME?
  ^doubles [^doubles [u0 u1 u2 u3]
            ^double v]
  [(/ u0 v)
   (/ u1 v)
   (/ u2 v)
   (/ u3 v)])

(defn vmag ;; cannot add ^double to return ??? FIXME
  [[^double x ^double y ^double z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn vnorm
  ^doubles [^doubles u]
  (let [mag (vmag u)]
    (if (= mag 1.0)
      u
      (if (= mag 0.0)
        [0.0 0.0 0.0]
        (div3 u mag)))))

;; NOTE! OpenGL assumes Column Major Matrices in memory.  The above
;; math is all in Row Major.  So, keep this guide in mind when
;; interpreting OpenGL code that accesses a matrix in memory.
;;
;; [ m01 m05 m09 m13
;;   m02 m06 m10 m14
;;   m03 m07 m11 m15
;;   m04 m08 m12 m16 ]

(defn translate
  ^doubles [^doubles m ^doubles[x y z]]
  (mmul m
        [1 0 0 x
         0 1 0 y
         0 0 1 z
         0 0 0 1]))

(defn rotate
  ^doubles [^doubles m ^double angle ^doubles [x y z]]
  (let [c       (Math/cos angle)
        l-c     (- 1.0 c)
        s       (Math/sin angle)
        [x y z] (vnorm [x y z])]
    (mmul m
          ;; man page:
          ;;[x^2(1-c)+c xy(1-c)-zs xz(1-c)+ys 0
          ;; yx(1-c)+zs y^2(1-c)+c yz(1-c)-xs 0
          ;; xz(1-c)-ys yz(1-c)+xs z^2(1-c)+c 0
          ;; 0          0          0          1]
          [(+ (* x x l-c) c)       (- (* x y l-c) (* z s)) (+ (* x z l-c) (* y s)) 0.0
           (+ (* y x l-c) (* z s)) (+ (* y y l-c) c)       (- (* y z l-c) (* x s)) 0.0
           (- (* x z l-c) (* y s)) (+ (* y z l-c) (* x s)) (+ (* z z l-c) c)       0.0
           0.0                     0.0                     0.0                     1.0]
          )))

(defn scale
  ^doubles [^doubles m ^doubles[x y z]]
  (mmul m
        [x 0 0 0
         0 y 0 0
         0 0 z 0
         0 0 0 1]))

(defn look-at
  "return a view matrix"
  ^doubles [^doubles eye ^doubles center ^doubles upv]
  (let [upv     (vnorm upv)
        forward (vnorm (vsub3 center eye))
        side    (vnorm (cross forward upv))
        upv     (cross side forward) ;; gluLookAt code has this
        ]
    (translate [(side 0)        (side 1)        (side 2)        0.0
                (upv 0)         (upv 1)         (upv 2)         0.0
                (- (forward 0)) (- (forward 1)) (- (forward 2)) 0.0
                0.0             0.0             0.0             1.0]
               (vmuls eye -1.0))))

(defn mpr [[m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33]]
  (println "[" m00 m01 m02 m03)
  (println " " m10 m11 m12 m13)
  (println " " m20 m21 m22 m23)
  (println " " m30 m31 m32 m33 "]"))
