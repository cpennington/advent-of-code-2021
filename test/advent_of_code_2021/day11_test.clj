(ns advent-of-code-2021.day11-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day11 :refer [do-1 do-2 sample step]]))

(deftest task-1
  (testing "Sample"
    (is (= 1656 (do-1 sample))))
  (testing "Solution"
    (is (= 1615 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 195 (do-2 sample))))
  (testing "Solution"
    (is (= 249 (do-2)))))
