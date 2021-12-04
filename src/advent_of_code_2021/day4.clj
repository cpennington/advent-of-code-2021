(ns advent-of-code-2021.day4
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]
   [clojure.set :as set]))


(def line-indexes [[0  1  2  3  4]
                   [5  6  7  8  9]
                   [10 11 12 13 14]
                   [15 16 17 18 19]
                   [20 21 22 23 24]
                   
                   [0 5 10 15 20]
                   [1 6 11 16 21]
                   [2 7 12 17 22]
                   [3 8 13 18 23]
                   [4 9 14 19 24]])

(defn board->line-set
  [board line-indexes]
  (->> line-indexes
      (map #(nth board %))
      set))

(defn board->line-sets
  [board]
  (->> line-indexes
      (map #(board->line-set board %))))

(defn parse-input
  [input]
  (let [[moves boards] input]
  {:moves moves
   :boards (->> boards
                (partition 25)
                (map board->line-sets))}))

(def sample (parse-input [[7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

                          [22 13 17 11  0
                           8  2 23  4 24
                           21  9 14 16  7
                           6 10  3 18  5
                           1 12 20 15 19

                           3 15  0  2 22
                           9 18 13 17  5
                           19  8  7 25 23
                           20 11 10 24  4
                           14 21 16 12  6

                           14 21 17 24  4
                           10 16 15  9 19
                           18  8 23 26 20
                           22 11 13  6  5
                           2  0 12  3  7]]))

(def input (-> (core/get-input 4)
               (str/split #"\n" 2)
               (#(map core/string->edn %))
               parse-input))

(defn mark-board
  [value board]
  (map #(disj % value) board))

(defn finished?
  [board]
  (->> board
       (map count)
       (some (partial = 0))
       true?))

(defn step-boards
  [{:keys [boards complete done moves] :or {boards [] complete [] done [] moves []}} ]
  (let [[next & rest] moves
        {unfinished false finished true} (->> boards
                                              (map (partial mark-board next))
                                              (group-by finished?))]
    {:boards unfinished
     :complete (into complete finished)
     :done (conj done next)
     :moves rest}))

(defn score-board
  [board]
  (->> (apply set/union board)
       (apply +)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> (iterate step-boards input)
        (filter (comp seq :complete))
        first
        ((fn [state] (* (-> state :done last) (-> state :complete first score-board))))
        )))

(defn do-2
  ([]
   (do-2 input))
  ([input]
     (->> (iterate step-boards input)
          (remove (comp seq :boards))
          first
          ((fn [state] (* (-> state :done last) (-> state :complete last score-board)))))))

(comment
  (do-2 sample)
  (->> (iterate step-boards sample)
       (remove (fn [state] (-> state :boards seq)))
       first)
  (into [] [[] ()]))
  