(ns advent-of-code-2021.day4-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.day4 :refer :all]))

(deftest task-1
  (testing "Sample"
    (is (= 4512 (do-1 sample))))
  (testing "Solution"
    (is (= 58838 (do-1) )))
  )

(deftest task-2
  (testing "Sample"
    (is (= 1924 (do-2 sample))))
  (testing "Solution"
    (is (= 6256 (do-2)))))
