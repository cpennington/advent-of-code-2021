(ns advent-of-code-2021.day7
  (:require
   [advent-of-code-2021.core :as core]))

(def sample [16,1,2,0,4,2,7,1,2,14])
(def input (core/get-input-as-edn 7))

;; Used for the original solution of task 1
;; (defn median
;;   [pop]
;;   (let [sorted (sort pop)
;;         size (count pop)]
;;     (nth sorted (/ size 2))))

(defn minimize
  [input]
  (let [step (fn [{:keys [pop upper-bound lower-bound cost-fn] :as input}]
               (let [ub (or upper-bound (apply max pop))
                     lb (or lower-bound 0)
                     target (int (/ (+ ub lb) 2))
                     lb-cost (cost-fn pop lb)
                     ub-cost (cost-fn pop ub)]
                 (if (< lb-cost ub-cost )
                   (assoc input :upper-bound (min target (- ub 1)))
                   (assoc input :lower-bound (max target (+ lb 1))))))]
    (->> (update input :pop sort)
         (iterate step)
         (drop 1)
         (take 1000)
         (drop-while #(not= (:upper-bound %) (:lower-bound %)))
         first
         (#((:cost-fn %) (:pop %) (:upper-bound %))))))

(defn linear-cost
  [pop target]
  (->> pop
       (map #(Math/abs (- % target)))
       (reduce +)))

(defn triangle-cost
  [pop target]
  (->> pop
       (map #(Math/abs (- % target)))
       (map #(/ (* % (+ 1 %)) 2))
       (reduce +)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (minimize {:pop input :cost-fn linear-cost})))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (minimize {:pop input :cost-fn triangle-cost})))

(comment
  (do-1 sample)
  (minimize {:pop sample :cost-fn triangle-cost})
  (apply max sample)
  )

