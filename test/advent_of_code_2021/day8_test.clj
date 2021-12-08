(ns advent-of-code-2021.day8-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day8 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 26 (do-1 sample))))
  (testing "Solution"
    (is (= 344 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 61229 (do-2 sample))))
  (testing "Solution"
    (is (= nil (do-2)))))
