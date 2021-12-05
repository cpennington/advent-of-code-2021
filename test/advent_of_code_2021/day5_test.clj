(ns advent-of-code-2021.day5-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day5 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 5 (do-1 sample))))
  (testing "Solution"
    (is (= 4728 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 12 (do-2 sample))))
  (testing "Solution"
    (is (= 17717 (do-2)))))
