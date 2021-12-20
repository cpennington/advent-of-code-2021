(ns advent-of-code-2021.day19-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day19 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 79 (do-1 sample))))
  (testing "Solution"
    (is (= 398 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 3621 (do-2 sample))))
  (testing "Solution"
    (is (= nil (do-2)))))
