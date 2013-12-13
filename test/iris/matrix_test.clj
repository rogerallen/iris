(ns iris.matrix-test
  (:require [clojure.test :refer :all]
            [iris.matrix :as mat]))

;; testing is way too skimpy...FIXME

(deftest matrix-1
  (testing "identity-matrix"
    (is (= (mat/identity-matrix)
           [1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0]))))

(deftest matrix-mul-1
  (testing "matrix mult"
    (is (= (mat/mmul
            [1 2 3 4
             5 6 7 8
             9 10 11 12
             13 14 15 16]
            [1 3 5 7
             9 11 13 15
             17 19 21 23
             25 27 29 31])
           [170 190 210 230
            378 430 482 534
            586 670 754 838
            794 910 1026 1142]))))
