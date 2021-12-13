(ns advent-of-code-2021.day13
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(defn parse-input
  [input]
  (let [{folds true points false}
        (->> input
             str/split-lines
             (mapv str/trim)
             (remove #(= "" %))
             (group-by #(str/starts-with? % "fold")))]
    {:points (set (map core/string->edn points))
     :folds (->> folds
                 (mapv #(re-find #"fold along (.*)=(\d*)" %))
                 (mapv (fn [[_ axis amount]]
                         [(keyword axis) (Integer/parseInt amount)])))}))
(def sample (parse-input
  "6,10
   0,14
   9,10
   0,3
   10,4
   4,11
   6,0
   6,12
   4,1
   0,13
   10,12
   3,4
   3,0
   8,4
   1,10
   2,14
   8,10
   9,0
   
   fold along y=7
   fold along x=5"))
(def input (parse-input (core/get-input 13)))

(defn fold-points
  [points [fold-axis fold-line]]
  (let [ix (case fold-axis
             :x 0
             :y 1)]
    (->> points
        (map (fn [pt]
               (if (< fold-line (nth pt ix))
                 (update pt ix #(- (* 2 fold-line) %))
                 pt)))
         set)))

(defn display
  [points]
  (let [max-x (apply max (map #(nth % 0) points))
        max-y (apply max (map #(nth % 1) points))
        min-x (apply min (map #(nth % 0) points))
        min-y (apply min (map #(nth % 1) points))
        grid (for [y (range (inc max-y))
                   x (range (inc max-x))]
               (if (contains? points [x y]) \# \.))]
    (prn min-x min-y max-x max-y)
    (->> (partition-all (inc max-x) grid)
         (map #(apply str %))
         (str/join "\n"))))

(defn do-1
  ([]
   (do-1 input))
  ([{:keys [points folds]}]
   (count (fold-points points (first folds)))))

(defn do-2
  ([]
  (do-2 input))
  ([{:keys [points folds]}]
   (display (reduce fold-points points folds))))

(comment
  (re-find #"fold along (.*)=(\d*)" "fold along x=8")
  sample
  (boolean "")
  ((fn [[_ axis amount]]
     [(keyword axis) (Integer/parseInt amount)]) ["fold along y=7" "y" "7"])
  (print (display (reduce fold-points (:points input) (:folds input))))
  (-> (:points sample)
      display
      print)
  (repeat 5 \.))

