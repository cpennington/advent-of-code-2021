(ns advent-of-code-2021.day21
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(defn parse-input
  [input]
  (-> input
      (str/replace #"Player \d starting position: " "")
      core/string->edn))

(def sample [4 8])
(def input (parse-input (core/get-input 21)))

(def rolls (->> (range 1 101)
                repeat
                (apply concat)
                (map vector (map inc (range)))))

(def roll-sums
  (->> rolls
       (partition 3)
       (map (fn [[[_ r1] [_ r2] [c3 r3]]]
              [c3 (+ r1 r2 r3)]))
       (map (fn [[count sum]] [count (mod sum 10)]))
       (partition 2)))

(defn move-piece
  [pos amt]
  (-> pos
      (+ amt)
      dec
      (mod 10)
      inc))

(defn step-game-1
  [{:keys [pos scores counts rolls] :or {scores [0 0] rolls roll-sums}}]
  (let [[[[p1-count p1-roll] [p2-count p2-roll]] & rolls] rolls
        pos (mapv move-piece pos [p1-roll p2-roll])
        next-counts [p1-count p2-count]
        next-scores (mapv + pos scores)]
    {:pos pos
     :prev-counts counts
     :prev-scores scores
     :scores next-scores
     :counts next-counts
     :rolls rolls})
  )

(defn score-state
  [{:keys [scores prev-scores counts prev-counts]}]
  (let [[p1-score p2-score] scores]
    (if (>= p1-score 1000)
      (* (peek prev-scores) (first counts))
      (* p1-score (peek counts)))))

(def dirac-3
  (frequencies (for [r1 [1 2 3]
                     r2 [1 2 3]
                     r3 [1 2 3]]
                 (+ r1 r2 r3))))

(defn step-state-2
  [[[pos score to-play] state-count]]
  (let [to-play' (- 1 to-play)
        future-states (for [[roll count] dirac-3]
                        (let [new-pos (move-piece (nth pos to-play) roll)
                              new-score (+ (nth score to-play) new-pos)]
                          {[(assoc pos to-play new-pos)
                            (assoc score to-play new-score)
                            to-play'] (* state-count count)}))]
    (apply merge-with + future-states)))

(defn winner-2?
  [[[_ [p1-score p2-score] _] _]]
  (cond
    (>= p1-score 21) :p1
    (>= p2-score 21) :p2
    :else nil))

(defn step-games-2
  [{:keys [incomplete] :as state}]
  (let [{next-incomplete nil
         p1-winners :p1
         p2-winners :p2}
        (->> incomplete
             (map step-state-2)
             (apply merge-with +)
             (group-by winner-2?))]
    (-> state
        (assoc :incomplete (into {} next-incomplete))
        (update :p1-win-count #(+ % (reduce + (map peek p1-winners))))
        (update :p2-win-count #(+ % (reduce + (map peek p2-winners)))))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (-> {:pos input}
      (->> (iterate step-game-1)
           (take 10000)
        ;;    (drop 200)
           (drop-while (fn [state] (-> state
                                       :scores
                                       ((partial every? #(< % 1000))))))
        ;;    (drop 34)
           )
      first
      (dissoc :rolls)
      score-state)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> {:incomplete {[input [0 0] 0] 1} :p1-win-count 0 :p2-win-count 0}
        (iterate step-games-2)
        (take 100)
        (drop-while (comp seq :incomplete))
        first
        (#(max (:p1-win-count %) (:p2-win-count %))))))

(comment
  dirac-3
  input
  (do-1 input)
  (do-2 input)
  (every? #(< % 1000) [1000 100])
  (-> {:pos sample}
      (->> (iterate step-game)
           (take 10000)
        ;;    (drop 200)
           (drop-while (fn [state] (-> state
                                       :scores
                                       ((partial every? #(< % 1000))))))
        ;;    (drop 34)
           )
      first
      (dissoc :rolls)
      score-state)
  (take 10 (drop 30 roll-sums))
  (assoc [0 1] 1 2)
  (->> {:incomplete {[[4 8] [0 0] 0] 1} :p1-win-count 0 :p2-win-count 0}
       (iterate step-games-2)
       (take 100)
       (drop-while (comp seq :incomplete))
       first)
  )

