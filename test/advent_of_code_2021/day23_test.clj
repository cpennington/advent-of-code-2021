(ns advent-of-code-2021.day23-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day23 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 12521 (do-1 sample))))
  (testing "Solution"
    (is (= 18195 (do-1) ))))

;; (deftest task-2
;;   (testing "Sample"
;;     (is (= nil (do-2 sample))))
;;   (testing "Solution"
;;     (is (= nil (do-2)))))
