(ns iris.perf-test
  (:require [clojure.test :refer :all]
            [iris.pipeline :as iris]
            [iris.matrix :as mat]
            [iris.util :as util]))

(defn single-fullscreen-triangle-setup
  []
  (let [dim 384]
    [dim
     [{:type     :triangle-list
       :vertices [{:x -1.0 :y -1.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                  {:x  3.0 :y -1.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                  {:x -1.0 :y  3.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}
                  ]}]
     {:viewport          [0 0 dim dim]
      :fbport            [0 0 dim dim]
      :depth-range       [0.0 1.0]
      :view-matrix       (mat/identity-matrix)
      :model-matrix      (mat/identity-matrix)
      :projection-matrix (mat/identity-matrix)
      }
     {:x      0
      :y      0
      :width  dim
      :height dim
      :data   (vec (repeat (* dim dim) {:r 0 :g 0 :b 0 :z 1}))
      }
     4220896307]))

;; FIXME - would be nice to have a macro for this
(deftest single-fullscreen-triangle
  (testing "single fullscreen triangle  no parallelism"
    (let [[dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/render-framebuffer state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-2
  (testing "single fullscreen triangle  2x parallelism"
    (let [n 2
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-3
  (testing "single fullscreen triangle  3x parallelism"
    (let [n 3
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-4
  (testing "single fullscreen triangle  4x parallelism"
    (let [n 4
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-6
  (testing "single fullscreen triangle  6x parallelism"
    (let [n 6
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-8
  (testing "single fullscreen triangle  8x parallelism"
    (let [n 8
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-12
  (testing "single fullscreen triangle 12x parallelism"
    (let [n 12
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-16
  (testing "single fullscreen triangle 16x parallelism"
    (let [n 16
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-24
  (testing "single fullscreen triangle 24x parallelism"
    (let [n 24
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest single-fullscreen-triangle-32
  (testing "single fullscreen triangle 32x parallelism"
    (let [n 32
          [dim objects state framebuffer the-crc] (single-fullscreen-triangle-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

;; ======================================================================
(defn many-triangles-setup
  []
  (let [dim 384
        cubes (apply
               vector
               (flatten
                (for [x (range -1.0 1.01 0.4)]
                  (for [y (range -1.0 1.01 0.4)]
                    (for [z (range -1.0 1.01 0.4)]
                      {:type   :cube
                       :center [x y z]
                       :x-size 0.1 :y-size 0.1 :z-size 0.1
                       :color  {:r (/ (inc x) 2) :g (/ (inc y) 2) :b (/ (inc z) 2)}
                       })))))]
    [dim
     cubes
     {:viewport          [0 0 dim dim]
      :fbport            [0 0 dim dim]
      :depth-range       [0.0 1.0]
      :light-vector      [0.5773502691896258 0.5773502691896258 -0.5773502691896258]
      :view-matrix       (mat/look-at [0.2 1.7 -5.0] [0.0 0.0 0.0] [0.0 1.0 0.0])
      :model-matrix      (mat/identity-matrix)
      :projection-matrix (mat/perspective (Math/toRadians 25) 1.0 1.0 10.0)
      }
     {:x      0
      :y      0
      :width  dim
      :height dim
      :data   (vec (repeat (* dim dim) {:r 0 :g 0 :b 0 :z 1}))
      }
     1165338380]))

;; FIXME - would be nice to have a macro for this
(deftest many-triangles
  (testing "216 cubes  no parallelism"
    (let [[dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/render-framebuffer state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-2
  (testing "216 cubes  2x parallelism"
    (let [n 2
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-3
  (testing "216 cubes  3x parallelism"
    (let [n 3
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-4
  (testing "216 cubes  4x parallelism"
    (let [n 4
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-6
  (testing "216 cubes  6x parallelism"
    (let [n 6
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-8
  (testing "216 cubes  8x parallelism"
    (let [n 8
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-12
  (testing "216 cubes 12x parallelism"
    (let [n 12
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-16
  (testing "216 cubes 16x parallelism"
    (let [n 16
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-24
  (testing "216 cubes 24x parallelism"
    (let [n 24
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

(deftest many-triangles-32
  (testing "216 cubes 32x parallelism"
    (let [n 32
          [dim objects state framebuffer the-crc] (many-triangles-setup)
          _ (print *testing-contexts* ": ")
          framebuffer (time (iris/parallel-render-framebuffer
                             n state framebuffer objects))
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc the-crc)))))

;; ======================================================================

(deftest perf-test-all
  (many-triangles)
  (many-triangles-2)
  (many-triangles-3)
  (many-triangles-4)
  (many-triangles-6)
  (many-triangles-8)
  (many-triangles-12)
  (many-triangles-16)
  (many-triangles-24)
  (many-triangles-32)

  (single-fullscreen-triangle)
  (single-fullscreen-triangle-2)
  (single-fullscreen-triangle-3)
  (single-fullscreen-triangle-4)
  (single-fullscreen-triangle-6)
  (single-fullscreen-triangle-8)
  (single-fullscreen-triangle-12)
  (single-fullscreen-triangle-16)
  (single-fullscreen-triangle-24)
  (single-fullscreen-triangle-32))

(defn test-ns-hook []
  (perf-test-all))
