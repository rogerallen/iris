(ns iris.matrix
  (:require [primitive-math :as p]))

;; a hand-made matrix library only suitable for use in a
;; self-contained project like this.  this is NOT trying to be a real
;; library.  Code was added as-needed.

;; Using https://github.com/ztellman/primitive-math to keep the math as I intend.
(p/use-primitive-operators)
;;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn identity-matrix
  "return a 4x4 identity matrix"
  ^doubles []
  (double-array [1.0 0.0 0.0 0.0
                 0.0 1.0 0.0 0.0
                 0.0 0.0 1.0 0.0
                 0.0 0.0 0.0 1.0]))

;; matrix format
;;[m00 m01 m02 m03
;; m10 m11 m12 m13
;; m20 m21 m22 m23
;; m30 m31 m32 m33]
(defn mmul
  "4x4 matrix multiplication M x N"
  ^doubles [^doubles m ^doubles n]
  ;; take first matrix by rows.  take second matrix by columns
  (let [m00 (aget m 0) n00  (aget n 0)
        m01 (aget m 1) n01  (aget n 1)
        m02 (aget m 2) n02  (aget n 2)
        m03 (aget m 3) n03  (aget n 3)
        m10 (aget m 4) n10  (aget n 4)
        m11 (aget m 5) n11  (aget n 5)
        m12 (aget m 6) n12  (aget n 6)
        m13 (aget m 7) n13  (aget n 7)
        m20 (aget m 8) n20  (aget n 8)
        m21 (aget m 9) n21  (aget n 9)
        m22 (aget m 10) n22 (aget n 10)
        m23 (aget m 11) n23 (aget n 11)
        m30 (aget m 12) n30 (aget n 12)
        m31 (aget m 13) n31 (aget n 13)
        m32 (aget m 14) n32 (aget n 14)
        m33 (aget m 15) n33 (aget n 15)]
    (double-array
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
      (+ (* m30 n03) (* m31 n13) (* m32 n23) (* m33 n33))])))

;; (mmul (identity-matrix) (identity-matrix))
;; (mmul (identity-matrix) [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])

(defn mvmul
  "4x4 matrix multiplication M x N"
  ^doubles [^doubles m ^doubles v]
  ;; vector is like a single column in 2nd matrix
(let [m00 (aget m 0)
      m01 (aget m 1)
      m02 (aget m 2)
      m03 (aget m 3)
      m10 (aget m 4)
      m11 (aget m 5)
      m12 (aget m 6)
      m13 (aget m 7)
      m20 (aget m 8)
      m21 (aget m 9)
      m22 (aget m 10)
      m23 (aget m 11)
      m30 (aget m 12)
      m31 (aget m 13)
      m32 (aget m 14)
      m33 (aget m 15)
      v0  (aget v 0)
      v1  (aget v 1)
      v2  (aget v 2)
      v3  (aget v 3)]
  (double-array
   [(+ (* m00 v0) (* m01 v1) (* m02 v2) (* m03 v3))
    (+ (* m10 v0) (* m11 v1) (* m12 v2) (* m13 v3))
    (+ (* m20 v0) (* m21 v1) (* m22 v2) (* m23 v3))
    (+ (* m30 v0) (* m31 v1) (* m32 v2) (* m33 v3))])))

;; (mvmul (identity-matrix) [1 2 3 4])

(defn vmuls
  "3d vector times scalar"
  ^doubles [^doubles u ^double s]
  (double-array [(* (aget u 0) s)
                 (* (aget u 1) s)
                 (* (aget u 2) s)]))

(defn vsub2
  "2d vector subtraction"
  ^doubles [^doubles u ^doubles v]
  (double-array [(- (aget u 0) (aget v 0))
                 (- (aget u 1) (aget v 1))]))

(defn vsub3
  "3d vector subtraction"
  ^doubles [^doubles u ^doubles v]
  (double-array [(- (aget u 0) (aget v 0))
                 (- (aget u 1) (aget v 1))
                 (- (aget u 2) (aget v 2))]))

(defn cross
  "3d vector cross product"
  ^doubles [^doubles u ^doubles v]
  (let [u0 (aget u 0) v0 (aget v 0)
        u1 (aget u 1) v1 (aget v 1)
        u2 (aget u 2) v2 (aget v 2)]
    (double-array
     [(- (* u1 v2) (* u2 v1))
      (- (* u2 v0) (* u0 v2))
      (- (* u0 v1) (* u1 v0))])))

(defn cross2s
  "3d vector cross product with 2D inputs, returning only the z component"
  ^double [^doubles u ^doubles v]
  (- (* (aget u 0) (aget v 1))
     (* (aget u 1) (aget v 0))))

(defn dot3
  "3d vector dot product"
  ^double [^doubles u ^doubles v]
  (+ (* (aget u 0) (aget v 0))
     (* (aget u 1) (aget v 1))
     (* (aget u 2) (aget v 2))))

(defn div3
  "3d vector divided by scalar"
  ^doubles [^doubles u ^double s]
  (double-array [(/ (aget u 0) s)
                 (/ (aget u 1) s)
                 (/ (aget u 2) s)]))

(defn div4
  "4d vector devided by scalar"
  ^doubles [^doubles u ^double s]
  (double-array [(/ (aget u 0) s)
                 (/ (aget u 1) s)
                 (/ (aget u 2) s)
                 (/ (aget u 3) s)]))

(defn vmag ;; cannot add ^double to return ??? FIXME
  "3d vector magnitude"
  ^double [^doubles u]
  (let [x (aget u 0)
        y (aget u 1)
        z (aget u 2)]
    (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn vnorm
  "3d vector normalization"
  ^doubles [^doubles u]
  (let [mag ^double (vmag u)]
    (if (= mag 1.0)
      u
      (if (= mag 0.0)
        (double-array [0.0 0.0 0.0])
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
  ^doubles [^doubles m ^doubles[x y z]]
  (mmul m
        (double-array
         [1 0 0 x
          0 1 0 y
          0 0 1 z
          0 0 0 1])))

(defn rotate
  "rotate 4x4 matrix by angle radians about a 3d vector, returning new matrix"
  ^doubles [^doubles m ^double angle ^doubles xyz]
  (let [c    (Math/cos angle)
        l-c  (- 1.0 c)
        s    (Math/sin angle)
        nxyz (vnorm xyz)
        x    (aget nxyz 0)
        y    (aget nxyz 1)
        z    (aget nxyz 2)]
    (mmul m
          ;; man page:
          ;;[x^2(1-c)+c xy(1-c)-zs xz(1-c)+ys 0
          ;; yx(1-c)+zs y^2(1-c)+c yz(1-c)-xs 0
          ;; xz(1-c)-ys yz(1-c)+xs z^2(1-c)+c 0
          ;; 0          0          0          1]
          (double-array
           [(+ (* x x l-c) c)       (- (* x y l-c) (* z s)) (+ (* x z l-c) (* y s)) 0.0
            (+ (* y x l-c) (* z s)) (+ (* y y l-c) c)       (- (* y z l-c) (* x s)) 0.0
            (- (* x z l-c) (* y s)) (+ (* y z l-c) (* x s)) (+ (* z z l-c) c)       0.0
            0.0                     0.0                     0.0                     1.0]))))

(defn scale
  "scale 4x4 matrix by 3d vector, returning new matrix"
  ^doubles [^doubles m ^doubles[x y z]]
  (mmul m
        (double-array
         [x 0 0 0
          0 y 0 0
          0 0 z 0
          0 0 0 1])))

(defn look-at
  "return a 4x4 view matrix from eye 3d vector, to center 3d vector
  with upv 3d vector indicating the 'up' direction."
  ^doubles
  [^doubles eye ^doubles center ^doubles upv]
  (let [upv     (vnorm upv)
        forward (vnorm (vsub3 center eye))
        side    (vnorm (cross forward upv))
        upv     (cross side forward) ;; gluLookAt code has this
        ]
    (translate (double-array
                [(aget side 0)        (aget side 1)        (aget side 2)        0.0
                 (aget upv 0)         (aget upv 1)         (aget upv 2)         0.0
                 (- (aget forward 0)) (- (aget forward 1)) (- (aget forward 2)) 0.0
                 0.0             0.0             0.0             1.0])
               (vmuls eye -1.0))))

(defn ortho
  "return 4x4 orthographic projection matrix"
  ^doubles [left right bottom top near far]
  (let [tx (- (/ (+ ^double right ^double left) (- ^double right ^double left)))
        ty (- (/ (+ ^double top ^double bottom) (- ^double top ^double bottom)))
        tz (- (/ (+ ^double far ^double near) (- ^double far ^double near)))
        sx (/ 2.0 (- ^double right ^double left))
        sy (/ 2.0 (- ^double top ^double bottom))
        sz (/ 2.0 (- ^double far ^double near))]
    (double-array
     [sx 0  0  tx
      0  sy 0  ty
      0  0  sz tz
      0  0  0  1])))

(defn frustum
  "return 4x4 perspective projection matrix"
  ^doubles [left right bottom top near far]
  (let [A (/ (+ ^double right ^double left) (- ^double right ^double left))
        B (/ (+ ^double top ^double bottom) (- ^double top ^double bottom))
        C (- (/ (+ ^double far ^double near) (- ^double far ^double near)))
        D (- (/ (* 2.0 ^double far ^double near) (- ^double far ^double near)))
        E (/ (* 2.0 ^double far ^double near) (- ^double right ^double left))
        F (/ (* 2.0 ^double far ^double near) (- ^double top ^double bottom))]
    (double-array
     [E 0  A 0
      0 F  B 0
      0 0  C D
      0 0 -1 0])))

(defn perspective
  "return 4x4 perspective projection matrix"
  ^doubles [^double fov-y ^double aspect ^double near ^double far]
  (let [f (/ (Math/cos (/ fov-y 2.0)) (Math/sin (/ fov-y 2.0))) ; cotangent
        C (- (/ (+ far near) (- far near)))
        D (- (/ (* 2.0 far near) (- far near)))
        F (/ f aspect)]
    (double-array
     [F 0  0 0
      0 f  0 0
      0 0  C D
      0 0 -1 0])))
