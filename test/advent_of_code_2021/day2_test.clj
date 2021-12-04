(ns advent-of-code-2021.day2-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day2 :refer [do-1 do-2]]))

(def sample '[forward 5
              down 5
              forward 8
              up 3
              down 8
              forward 2])

(deftest task-1
  (testing "Solution"
    (is (= 1692075 (do-1))))
  (testing "Sample"
    (is (= 150 (do-1 sample)))))

(deftest task-2
  (testing "Solution"
    (is (= 1749524700 (do-2))))
  (testing "Sample"
    (is (= 900 (do-2 sample)))))