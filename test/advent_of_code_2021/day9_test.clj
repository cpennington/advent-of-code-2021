(ns advent-of-code-2021.day9-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day9 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 15 (do-1 sample))))
  (testing "Solution"
    (is (= 607 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 1134 (do-2 sample))))
  (testing "Solution"
    (is (= 900864 (do-2)))))
