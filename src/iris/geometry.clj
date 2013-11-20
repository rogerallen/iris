(ns iris.geometry)

;;  y1 34   6  . 6   43
;;        f    .    b
;;  y0 1   25  . 52   1
;;    x0   x1

(defn evaluate-cube
  [object]
  (let [[cx cy cz] (:center object)
        dx (/ (:x-size object) 2)
        dy (/ (:y-size object) 2)
        dz (/ (:z-size object) 2)
        {:keys [r g b]} (:color object)
        x0 (- cx dx)
        x1 (+ cx dx)
        y0 (- cy dy)
        y1 (+ cy dy)
        z0 (- cz dz)
        z1 (+ cz dz)]
  [;; front
   {:x x0 :y y0 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   {:x x1 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 0 :nz -1}
   ;; back
   {:x x1 :y y0 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   {:x x0 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 0 :nz 1}
   ;; top
   {:x x0 :y y0 :z z0 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   {:x x1 :y y0 :z z1 :r r :g g :b b :nx 0 :ny -1 :nz 0}
   ;; bottom
   {:x x1 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   {:x x0 :y y1 :z z1 :r r :g g :b b :nx 0 :ny 1 :nz 0}
   ;; left
   {:x x0 :y y0 :z z0 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   {:x x0 :y y1 :z z0 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   {:x x0 :y y0 :z z1 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   {:x x0 :y y1 :z z1 :r r :g g :b b :nx -1 :ny 0 :nz 0}
   ;; right
   {:x x1 :y y0 :z z1 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   {:x x1 :y y1 :z z1 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   {:x x1 :y y0 :z z0 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   {:x x1 :y y1 :z z0 :r r :g g :b b :nx 1 :ny 0 :nz 0}
   ]))

(defn evaluate-object
  "an object is a map with a :type and associated data.  Output
  triangles for processing by a vertex shader"
  [object]
  (do
    ;;(println "evaluate" object)
    (case (:type object)
      :triangle-list (:vertices object)
      :cube (evaluate-cube object)
      ;; TODO sphere, etc.  add some things with normals
      (assert false))))
