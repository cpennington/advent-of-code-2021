(ns advent-of-code-2021.day10
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(def sample (str/split-lines (core/get-sample 10)))
(def input (str/split-lines (core/get-input 10)))

(defn is-open?
  [char]
  (-> char
      ((set "([<{"))
      boolean))

(def closers {\[ \]
              \( \)
              \< \>
              \{ \}})

(defn match?
  [left right]
  (= right (closers left)))

(defn check-next
  [{:keys [stack remaining]
    :or {stack [] remaining ""}
    :as input}]
  (cond
    (and (empty? remaining)
         (empty? stack)) (assoc input
                                :result :pass)
    (empty? stack) (assoc input
                          :stack [(first remaining)]
                          :remaining (rest remaining))
    (empty? remaining) (assoc input
                              :result :incomplete)
    :else (let [top (last stack)
                next (first remaining)]
            (cond
              (is-open? next) (assoc input
                                     :stack (conj stack next)
                                     :remaining (rest remaining))
              (match? top next) (assoc input
                                       :stack (pop stack)
                                       :remaining (rest remaining))
              :else (assoc input
                           :result :corrupted)))))

(defn process-line
  [input]
  (->> {:remaining input}
       (iterate check-next)
       (drop-while (comp nil? :result))
       first))

(def task-1-scores {\) 3
                    \] 57
                    \} 1197
                    \> 25137})

(def task-2-scores {\) 1
                    \] 2
                    \} 3
                    \> 4})

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (map process-line)
        (filter #(= :corrupted (:result %)))
        (map #(->> %
                   :remaining
                   first
                   task-1-scores))
        (reduce + 0))))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   (->> input
        (map process-line)
        (filter #(= :incomplete (:result %)))
        (map #(->> %
                   :stack
                   (map closers)
                   reverse
                   (reduce (fn [acc char] (+ (* 5 acc) (task-2-scores char))) 0)))
        sort
        (#(nth % (/ (count %) 2))))))

(comment
  (->> {:remaining "{([(<{}[<>[]}>{[]{[(<()>"}
       (iterate check-next)
       (drop-while (comp nil? :result))
       first
       (comp first :remaining))
  (match? \( \))
  (->> sample
       (map process-line)
       (filter #(= :incomplete (:result %)))
       (map #(->> %
                  :stack
                  (map closers)
                  reverse
                  (reduce (fn [acc char] (+ (* 5 acc) (task-2-scores char))) 0)))
       sort
       (#(nth % (/ (count %) 2)))))



