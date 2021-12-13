(ns advent-of-code-2021.day13-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2021.day13 :refer [do-1 do-2 sample]]))

(deftest task-1
  (testing "Sample"
    (is (= 17 (do-1 sample))))
  (testing "Solution"
    (is (= 847 (do-1) ))))

(deftest task-2
  (testing "Sample"
    (is (= "#####
#...#
#...#
#...#
#####" (do-2 sample))))
  (testing "Solution"
    (is (= "###...##..####.###...##..####..##..###.
#..#.#..#....#.#..#.#..#.#....#..#.#..#
###..#......#..#..#.#....###..#..#.###.
#..#.#.....#...###..#....#....####.#..#
#..#.#..#.#....#.#..#..#.#....#..#.#..#
###...##..####.#..#..##..####.#..#.###." (do-2)))))
