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

(deftest single-fullscreen-triangle-all
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
  (single-fullscreen-triangle-all))
