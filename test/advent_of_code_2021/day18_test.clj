(ns advent-of-code-2021.day18-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day18 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 4140 (do-1 sample))))
  (testing "Solution"
    (is (= 4365 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 3993 (do-2 sample))))
  (testing "Solution"
    (is (= 4490 (do-2)))))
