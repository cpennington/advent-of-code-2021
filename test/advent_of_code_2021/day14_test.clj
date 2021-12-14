(ns advent-of-code-2021.day14-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day14 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 1588 (do-1 sample))))
  (testing "Solution"
    (is (= 2703 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 2188189693529 (do-2 sample))))
  (testing "Solution"
    (is (= 2984946368465 (do-2)))))
