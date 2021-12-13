(ns advent-of-code-2021.day12-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day12 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 226 (do-1 sample))))
  (testing "Solution"
    (is (= 4885 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 3509 (do-2 sample))))
  (testing "Solution"
    (is (= 117095 (do-2)))))
