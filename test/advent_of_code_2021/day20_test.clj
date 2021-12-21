(ns advent-of-code-2021.day20-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day20 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 35 (do-1 sample))))
  (testing "Solution"
    (is (= 5680 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 3351 (do-2 sample))))
  (testing "Solution"
    (is (= 19766 (do-2)))))
