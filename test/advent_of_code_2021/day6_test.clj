(ns advent-of-code-2021.day6-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day6 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 5934 (do-1 sample))))
  (testing "Solution"
    (is (= 360610 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 26984457539 (do-2 sample))))
  (testing "Solution"
    (is (= 1631629590423 (do-2)))))
