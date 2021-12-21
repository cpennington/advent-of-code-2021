(ns advent-of-code-2021.day21-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day21 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 739785 (do-1 sample))))
  (testing "Solution"
    (is (= 576600 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 444356092776315 (do-2 sample))))
  (testing "Solution"
    (is (= 131888061854776 (do-2)))))
