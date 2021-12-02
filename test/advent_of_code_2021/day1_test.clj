(ns advent-of-code-2021.day1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.day1 :refer :all]))

(def sample [199
             200
             208
             210
             200
             207
             240
             269
             260
             263])

(deftest task-1
  (testing "Solution"
    (is (= (do-1) 1446)))
  (testing "Example 1"
    (is (= (do-1 sample) 7))))

(deftest task-2
  (testing "Solution"
    (is (= (do-2) 1486)))
  (testing "Sample"
    (is (= (do-2 sample) 5))))
