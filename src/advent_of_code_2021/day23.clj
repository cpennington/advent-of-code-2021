(ns advent-of-code-2021.day23
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [advent-of-code-2021.search :as search]
   [clojure.set :as set]))

(defn ->hallway
  [ix]
  [:hallway ix])

(defn ->room
  [label ix]
  [:room label ix])

(def mini-sample {:map-def {:hall-size 7
                            :rooms {\A {:size 2 :door 2}
                                    \B {:size 2 :door 4}}}
                  :pieces {(->room \A 0) \B
                           (->room \A 1) \B
                           (->room \B 0) \A
                           (->room \B 1) \A}})

(def sample {:map-def {:hall-size 11
                       :rooms {\A {:size 2 :door 2}
                               \B {:size 2 :door 4}
                               \C {:size 2 :door 6}
                               \D {:size 2 :door 8}}}
             :pieces {(->room \A 0) \B
                      (->room \A 1) \A
                      (->room \B 0) \C
                      (->room \B 1) \D
                      (->room \C 0) \B
                      (->room \C 1) \C
                      (->room \D 0) \D
                      (->room \D 1) \A}})
(def input {:map-def {:hall-size 11
                      :rooms {\A {:size 2 :door 2}
                              \B {:size 2 :door 4}
                              \C {:size 2 :door 6}
                              \D {:size 2 :door 8}}}
            :pieces {(->room \A 0) \A
                     (->room \A 1) \D
                     (->room \B 0) \C
                     (->room \B 1) \D
                     (->room \C 0) \B
                     (->room \C 1) \B
                     (->room \D 0) \A
                     (->room \D 1) \C}})

(defn part-2-input
  [input]
  (-> input
      (assoc-in [:map-def :rooms \A :size] 4)
      (assoc-in [:map-def :rooms \B :size] 4)
      (assoc-in [:map-def :rooms \C :size] 4)
      (assoc-in [:map-def :rooms \D :size] 4)
      (assoc-in [:pieces (->room \A 2)] \D)
      (assoc-in [:pieces (->room \B 2)] \C)
      (assoc-in [:pieces (->room \C 2)] \B)
      (assoc-in [:pieces (->room \D 2)] \A)
      (assoc-in [:pieces (->room \A 3)] \D)
      (assoc-in [:pieces (->room \B 3)] \B)
      (assoc-in [:pieces (->room \C 3)] \A)
      (assoc-in [:pieces (->room \D 3)] \C)
      (assoc-in [:pieces (->room \A 4)] (get-in input [:pieces (->room \A 1)]))
      (assoc-in [:pieces (->room \B 4)] (get-in input [:pieces (->room \B 1)]))
      (assoc-in [:pieces (->room \C 4)] (get-in input [:pieces (->room \C 1)]))
      (assoc-in [:pieces (->room \D 4)] (get-in input [:pieces (->room \D 1)]))))

(defn occupied?
  [pieces location]
  (contains? pieces location))

(def loc->ix peek)
(def loc->type first)
(def loc->label #(nth % 1))


(defn piece-cost
  [piece]
  (case piece
    \A 1
    \B 10
    \C 100
    \D 1000))

(defn add-map-fns
  [{:keys [map-def] :as search-state}]
  (let [rooms (:rooms map-def)]
    (letfn [(path-between
              [a b]
              (into #{}
                    (if (= a b)
                      [a]
                      (case [(loc->type a) (loc->type b)]
                        [:hallway :hallway]
                        (let [a-ix (loc->ix a)
                              b-ix (loc->ix b)]
                          (map ->hallway
                               (range (min a-ix b-ix) (inc (max a-ix b-ix)))))

                        [:room :room]
                        (let [a-label (loc->label a)
                              a-ix (loc->ix a)
                              b-label (loc->label b)
                              b-ix (loc->ix b)]
                          (if (= a-label b-label)
                            (map #(->room a-label %)
                                 (range (min a-ix b-ix) (inc (max a-ix b-ix))))
                            (let [a-to-door (path-between a (->room a-label 0))
                                  b-to-door (path-between b (->room b-label 0))
                                  a-door (-> rooms (#(get % a-label)) :door)
                                  b-door (-> rooms (#(get % b-label)) :door)
                                  door-to-door (path-between (->hallway a-door) (->hallway b-door))]
                              (concat a-to-door b-to-door door-to-door))))

                        [:hallway :room]
                        (let [[_ label _] b
                              to-door (path-between b (->room label 0))
                              door (-> rooms (#(get % label)) :door)
                              door-to-hall (path-between a (->hallway door))]
                          (concat to-door door-to-hall))

                        [:room :hallway]
                        (path-between b a)))))]
      (let [path-between-memo (memoize path-between)]
        (assoc-in search-state
                  [:map-def :path-between] path-between-memo)))))

(defn pieces-in-room
 [{:keys [rooms]} pieces room-label]
 (->> (get rooms room-label)
      :size
      range
      (map #(->room room-label %))
      (map #(get pieces %))
      (remove nil?)
      set))

(defn path-open?
  [{:keys [path-between]} pieces a b]
  (let [path (-> (path-between a b)
                 (disj a))
        occupied (->> pieces
                      keys)]
    (every? #(not (contains? path %)) occupied)))

(defn distance
  [{:keys [path-between]} a b]
  (dec (count (path-between a b))))

(defn path-cost
  [map-def piece a b]
  (* (piece-cost piece) (distance map-def a b)))

(defn possible-moves
  [{:keys [rooms hall-size] :as map-def} pieces]
  (let [room-size (-> rooms (#(get % \A)) :size)
        doors (->> rooms vals (map :door) set)]
    (for [[loc p] pieces
          :when (or (= (loc->type loc) :room)
                    (-> (pieces-in-room map-def pieces p)
                        (disj p)
                        empty?))
          loc' (into (map #(->room p %)
                          (range 0 room-size))
                     (when (= (loc->type loc) :room)
                       (->> hall-size
                            range
                            (remove doors)
                            (map #(->hallway %)))))
          :when (and (not= loc loc')
                     (path-open? map-def pieces loc loc'))]
      [(-> pieces
           (assoc loc' p)
           (dissoc loc))
       (path-cost map-def p loc loc')])))

(defn estimate-cost
  [map-def pieces]
  (->> pieces
       (map (fn [[loc label]]
              (* (piece-cost label)
                 (min (distance map-def (->room label 0) loc)
                      (distance map-def (->room label 1) loc)))))
       (reduce + 0)))

(defn setup-search
  [{:keys [map-def pieces]}]
  (search/setup
   {:initial-states [pieces]
    :neighbor-fn #(possible-moves map-def %)
    :est-fn #(estimate-cost map-def %)}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        add-map-fns
        setup-search
        (iterate search/explore-next)
        (take 100000)
        (drop-while (comp empty? :found))
        first
        :found
        first
        peek
        :total)))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   nil))

(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8080)
  (prof/profile (do-1 sample))
  (prof/profile (->> input
                     part-2-input
                     add-map-fns
                     setup-search
      ;; :neighbor-fn
      ;; (#(% (:initial-states (first (setup-search sample)))))
                     (iterate search/explore-next)
                     (take 200000)
                    ;;  (drop 10000)
                     (drop-while (comp empty? :found))
                     first
                     (#(assoc % :next (take 5 (:frontier %))))
                     (#(update % :frontier count))
                     (#(update % :visited count))
                     clojure.pprint/pprint))
  (part-2-input sample))