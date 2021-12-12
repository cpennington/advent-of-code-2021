(ns advent-of-code-2021.day10-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day10 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 26397 (do-1 sample))))
  (testing "Solution"
    (is (= 442131 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 288957 (do-2 sample))))
  (testing "Solution"
    (is (= 3646451424 (do-2)))))
