(ns advent-of-code-2021.day5
  (:require
   [advent-of-code-2021.core :as core]))

(def sample (partition-all 5 '[0,9 -> 5,9
                               8,0 -> 0,8
                               9,4 -> 3,4
                               2,2 -> 2,1
                               7,0 -> 7,4
                               6,4 -> 2,0
                               0,9 -> 2,9
                               3,4 -> 1,4
                               0,0 -> 8,8
                               5,5 -> 8,2]))
(def input (->> (core/get-input-as-edn 5)
                (partition-all 5)))

(defn vertical
  [[x1 _ _ x2 _]]
  (= x1 x2))

(defn horizontal
  [[_ y1 _ _ y2]]
  (= y1 y2))

(defn reversable-range
  [a b]
  (if (< a b)
    (range a (+ b 1))
    (reverse (range b (+ a 1)))))

(defn line->pts
  [line]
  (let [[x1 y1 _ x2 y2] line
        pts (cond
              (vertical line) (for [y (reversable-range y1 y2)] [[x1 y] 1])
              (horizontal line) (for [x (reversable-range x1 x2)] [[x y1] 1])
              :else (for [pt (map vector
                                  (reversable-range x1 x2)
                                  (reversable-range y1 y2))] [pt 1]))]
    (into {} pts)))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   (->> input
        (map line->pts)
        (apply merge-with +)
        vals
        (filter #(> % 1))
        count)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (filter #(or (horizontal %) (vertical %)))
        do-2)))

(comment
  sample
  (range 1 3)
  (for [y (range 1 (+ 3 1))] [[0 y] 1])
  (line->pts [1 1 '-> 1 3])
  (do-1 sample)
  (->> sample
       (filter #(or (horizontal %) (vertical %)))
       (map line->pts)
       (apply merge-with +))
  (reversable-range 5 1)
  (map vector
       (reversable-range 9 7)
       (reversable-range 7 9)))
