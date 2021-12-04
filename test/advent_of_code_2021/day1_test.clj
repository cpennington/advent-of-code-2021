(ns advent-of-code-2021.day1-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day1 :refer [do-1 do-2]]))

(def sample [199
             200
             208
             210
             200
             207
             240
             269
             260
             263])

(deftest task-1
  (testing "Solution"
    (is (= 1446 (do-1) )))
  (testing "Example 1"
    (is (= 7 (do-1 sample)))))

(deftest task-2
  (testing "Solution"
    (is (= 1486 (do-2))))
  (testing "Sample"
    (is (= 5 (do-2 sample)))))
