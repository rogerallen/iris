(ns iris.matrix-test
  (:require [clojure.test :refer :all]
            [iris.matrix :as mat])
  (:import  [iris.matrix Matrix4x4 Vector2 Vector3 Vector4]))

;; testing is way too skimpy...FIXME

(deftest matrix-1
  (testing "identity-matrix"
    (is (= (mat/identity-matrix)
           (Matrix4x4. 1.0 0.0 0.0 0.0
                       0.0 1.0 0.0 0.0
                       0.0 0.0 1.0 0.0
                       0.0 0.0 0.0 1.0)))))

(deftest matrix-mul-1
  (testing "matrix mult"
    (is (= (mat/mmul
            (Matrix4x4. 1 2 3 4
                        5 6 7 8
                        9 10 11 12
                        13 14 15 16)
            (Matrix4x4. 1 3 5 7
                        9 11 13 15
                        17 19 21 23
                        25 27 29 31))
           (Matrix4x4. 170 190 210 230
                       378 430 482 534
                       586 670 754 838
                       794 910 1026 1142)))))

;;(println (mat/vmuls (Vector3. 1.0 2.0 3.0) 3.0))

(deftest vmul-1
  (testing "vector scalar mul"
    (is (= (mat/vmuls (Vector3. 1 2 3) 3.0)
           (Vector3. 3 6 9)))))

(deftest vmul-2
  (testing "vector scalar mul"
    (is (= (mat/vmul2s (Vector2. 1 2) 3.0)
           (Vector2. 3 6)))))

(deftest vsub-1
  (testing "vector subtract"
    (is (= (mat/vsub2
            (Vector2. 10 20)
            (Vector2. 5 5))
           (Vector2. 5 15)))))
