(ns advent-of-code-2021.day7-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day7 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 37 (do-1 sample))))
  (testing "Solution"
    (is (= 352707 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 168 (do-2 sample))))
  (testing "Solution"
    (is (= 95519693 (do-2)))))
