(ns iris.core-test
  (:require [clojure.test :refer :all]
            [iris.pipeline :as iris]
            [iris.matrix :as mat]
            [iris.util :as util])
  (:import  [iris.matrix Matrix4x4 Vector2 Vector3 Vector4]))

(deftest simple-test
  (testing "simple usage of iris"
    (let [dim 64
          objects [{:type     :triangle-list
                    :vertices [{:x -1.0 :y -1.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                               {:x  1.0 :y -1.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                               {:x  1.0 :y  1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}
                               ;; fullscreen tri
                               {:x -3.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y  3.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               ]}]
          state {:viewport          (Vector4. 0 0 dim dim)
                 :fbport            (Vector4. 0 0 dim dim)
                 :depth-range       (Vector2. 0.0 1.0)
                 :view-matrix       (mat/identity-matrix)
                 :model-matrix      (mat/identity-matrix)
                 :projection-matrix (mat/identity-matrix)
                 }
          framebuffer {:x      0
                       :y      0
                       :width  dim
                       :height dim
                       :data   (vec (repeat (* dim dim)
                                            {:r 0 :g 0 :b 0 :z 1}))
                       }
          framebuffer (iris/render-framebuffer state framebuffer objects)
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc 3771693458)))))

(deftest simple-two-obj
  (testing "simple 2 object usage of iris"
    (let [dim 64
          objects [{:type     :triangle-list
                    :vertices [{:x -1.0 :y -1.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                               {:x  1.0 :y -1.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                               {:x  1.0 :y  1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}
                               ]}
                   {:type     :triangle-list
                    :vertices [{:x -3.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y  3.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               ]}]
          state {:viewport          (Vector4. 0 0 dim dim)
                 :fbport            (Vector4. 0 0 dim dim)
                 :depth-range       (Vector2. 0.0 1.0)
                 :view-matrix       (mat/identity-matrix)
                 :model-matrix      (mat/identity-matrix)
                 :projection-matrix (mat/identity-matrix)
                 }
          framebuffer {:x      0
                       :y      0
                       :width  dim
                       :height dim
                       :data   (vec (repeat (* dim dim)
                                            {:r 0 :g 0 :b 0 :z 1}))
                       }
          framebuffer (iris/render-framebuffer state framebuffer objects)
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc 3771693458)))))

(deftest parallel-test
  (testing "parallel usage of iris"
    (let [n 4
          dim 64
          sub-dim (/ dim n)
          objects [{:type     :triangle-list
                    :vertices [{:x -1.0 :y -1.0 :z 0.0 :r 1.0 :g 0.0 :b 0.0}
                               {:x  1.0 :y -1.0 :z 0.0 :r 0.0 :g 1.0 :b 0.0}
                               {:x  1.0 :y  1.0 :z 0.0 :r 0.0 :g 0.0 :b 1.0}
                               ;; fullscreen tri
                               {:x -3.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y -1.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               {:x  1.0 :y  3.0 :z 0.5 :r 1.0 :g 1.0 :b 1.0}
                               ]}]
          state {:viewport          (Vector4. 0 0 dim dim)
                 :fbport            (Vector4. 0 0 dim dim)
                 :depth-range       (Vector2. 0.0 1.0)
                 :view-matrix       (mat/identity-matrix)
                 :model-matrix      (mat/identity-matrix)
                 :projection-matrix (mat/identity-matrix)
                 }
          framebuffer {:x      0
                       :y      0
                       :width  dim
                       :height dim
                       ;; parallel-render-framebuffer does alloc
                       }
          framebuffer (iris/parallel-render-framebuffer n
                                                        state
                                                        framebuffer
                                                        objects)
          ;;_ (util/print-fb-to-ppm framebuffer)
          crc (util/check-crc framebuffer)]
      (is (= crc 3771693458)))))
