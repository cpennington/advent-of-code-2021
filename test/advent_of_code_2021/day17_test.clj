(ns advent-of-code-2021.day17-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day17 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 45 (do-1 sample))))
  (testing "Solution"
    (is (= 11781 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 112 (do-2 sample))))
  (testing "Solution"
    (is (= 4531 (do-2)))))
