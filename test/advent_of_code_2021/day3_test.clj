(ns advent-of-code-2021.day3-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent-of-code-2021.day3 :refer :all]))

(def sample '["00100"
              "11110"
              "10110"
              "10111"
              "10101"
              "01111"
              "00111"
              "11100"
              "10000"
              "11001"
              "00010"
              "01010"])

(deftest task-1
  (testing "Solution"
    (is (= 2261546 (do-1))))
  (testing "Sample"
    (is (= 198 (do-1 sample)))))

(deftest task-2
  (testing "Solution"
    (is (= 6775520 (do-2))))
  (testing "Sample"
    (is (= 230 (do-2 sample)))))

(comment
  sample)