(ns iris.geometry)

(defn evaluate-object
  "an object is a map with a :type and associated data.  Output
  triangles for processing by a vertex shader"
  [object]
  (do
    ;;(println "evaluate" object)
    (case (:type object)
      :triangle-list (:vertices object)
      ;; TODO sphere, cube, etc.  add some things with normals
      (assert false))))
