(ns advent-of-code-2021.day8
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]
   [clojure.core.logic :as lg]
   [clojure.set :as set]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map #(str/split % #"\|"))
       (map (fn [io]
              (map #(-> %
                        str/trim
                        (str/split #" ")) io)))))

(def sample (parse-input (core/get-sample 8)))
(def input (parse-input (core/get-input 8)))
(def all-letters (set "abcdefg"))

(defn on
  [seg ex]
  (lg/membero seg (vec ex)))

(defn off
  [seg ex]
  (lg/membero seg (vec (set/difference all-letters (set ex)))))

(defn na
  [_ _]
  lg/succeed)

;;   1 1
;; 2     3
;; 2     3
;;   4 4 
;; 5     6
;; 5     6
;;   7 7

(def digit-map
  {[1 2 3 5 6 7] 0
   [3 6] 1
   [1 3 4 5 7] 2
   [1 3 4 6 7] 3
   [2 3 4 6] 4
   [1 2 4 6 7] 5
   [1 2 4 5 6 7] 6
   [1 3 6] 7
   [1 2 3 4 5 6 7] 8
   [1 2 3 4 6 7] 9})

(def one   [off na  on  na  na  on  na])
(def two   [na  off na  na  na  off na])
(def three [na  off na  na  off na  na])
(def four  [off na  na  na  off na  off])
(def five  [na  na  off na  off na  na])
(def six   [na  na  off na  na  na  na])
(def seven [on  na  on  na  na  on  na])
(def eight [na  na  na  na  na  na  na])
(def nine  [na  na  na  na  off na  na])
(def zero  [na  na  na  off na  na  na])

(defn is
  [digit segs ex]
  ;; (if (= (->> digit (filter #{on}) count) (count ex))
    (lg/and* (mapv #(%1 %2 ex) digit segs))
    ;; lg/fail)
)

(defn some-digit
  [segs ex]
  (lg/or* (mapv #(is % segs ex) (case (count ex)
                                  2 [one]
                                  3 [seven]
                                  4 [four]
                                  5 [two three five]
                                  6 [six nine zero]
                                  7 [eight])
                ;; (filter #(= (->> % (filter #{on}) count) (count ex))
                        ;; [one two three four five six seven eight nine zero]
                        ;; )
                )))

(defn segments
  [examples]
  (let [segs (repeatedly 7 lg/lvar)]
    (lg/run 1 [q]
            (lg/distincto segs)
            (lg/== q segs)
            (lg/and* (mapv #(some-digit segs %) (sort-by
                                                 #(min (count %) (- 7 (count %)))
                                                ;;  count
                                                 examples))))))

(defn solve-line
  [[input output]]
  (let [[segment-chars] (segments input)
        char-map (into {} (map vector segment-chars (range 1 8)))
        mapped-input (map #(->> % (map char-map) sort) input)
        mapped-output (map #(->> % (map char-map) sort) output)
        input-digits (map digit-map mapped-input)
        output-digits (map digit-map mapped-output)
        ]
    ;; (prn segment-chars)
    ;; (prn input)
    ;; (prn mapped-input)
    ;; (prn input-digits)
    ;; (prn output)
    ;; (prn mapped-output)
    ;; (prn output-digits)
    (Integer/parseInt (str/join output-digits))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (map last)
        (apply concat)
        (into [])
        (map count)
        (filter #{2 3 4 7})
        count)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
       (map solve-line)
       (reduce +))))

(comment
  (set/difference all-letters (set "eg"))
  (do-1 sample)
  (->> one (filter #{on}) count)
  (is one (repeatedly lg/lvar 7) "eg")
  (mapv #(prn %1 %2) "abc" "def")
  (mapv #(%1 %2 "def") one [repeatedly 7 lg/lvar])
  (segments (-> sample first first))
  (mapv #(some-digit [repeatedly 7 lg/lvar] %) (-> sample first first))
  (->> one (filter #{on}) count)
  (segments ["ab"])
  (solve-line (-> sample (nth 2)))
  (str/join [1 3 4])
  (let [segs (repeatedly 7 lg/lvar)]
    (lg/run 5 [q]
            (lg/== q segs)
            (lg/== q [\a \c \b \d \f \e \g])
            (is two segs "abcde")))
  (time (do-2 input)))

