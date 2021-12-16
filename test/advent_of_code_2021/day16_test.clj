(ns advent-of-code-2021.day16-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day16 :refer [do-1 do-2 sample read-packet bitstream]]))

(deftest parsing
  (testing "literal"
    (is (= [{:version 6 :type 4 :contents 2021} {:bits '(0 0 0), :pos 21}]
           (read-packet (bitstream "D2FE28")))))
  (testing "length operator"
    (is (= [{:version 1, :type 6, :contents [{:version 6, :type 4, :contents 10}
                                             {:version 2, :type 4, :contents 20}]}
            {:bits '(0 0 0 0 0 0 0), :pos 49}]
           (read-packet (bitstream "38006F45291200")))))
  
  (testing "count operator"
    (is (= [{:version 7, :type 3, :contents [{:version 2, :type 4, :contents 1}
                                             {:version 4, :type 4, :contents 2}
                                             {:version 1, :type 4, :contents 3}]}
            {:bits '(0 0 0 0 0), :pos 51}]
           (read-packet (bitstream "EE00D40C823060"))))))

(deftest task-1
  (testing "Sample"
    (is (= 31 (do-1 sample))))
  (testing "Solution"
    (is (= 860 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= 1 (do-2 (bitstream "9C0141080250320F1802104A08")))))
  (testing "Solution"
    (is (= 470949537659 (do-2)))))
