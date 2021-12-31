(ns advent-of-code-2021.day23
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [advent-of-code-2021.search :as search]
   [clojure.set :as set]))

(defn ->hallway
  [ix]
  (inc ix))

(def offset-mult 4)

(defn ->room
  [label ix]
  (let [offset (case label
                 \A 0
                 \B 1
                 \C 2
                 \D 3)]
    (- (+ (* offset-mult offset) ix))))

(defn loc->ix
  [loc]
  (if (pos? loc)
    (dec loc)
    (mod (- loc) offset-mult)))

(defn loc->type
  [loc]
  (if (pos? loc)
    :hallway
    :room))

(defn loc->label
  [loc]
  (-> (- loc)
      (Math/floorDiv offset-mult)
      (+ (int \A))
      char))

(defn ix->str-ix
  [ix]
  (+ ix 15))

(def str-ixs (range 27))
(defn str-ix->ix
  [str-ix]
  (- str-ix 15))

(defn loc->piece
  [pieces loc]
  (if (string? pieces)
    (get pieces (ix->str-ix loc))
    (get pieces loc)))

(defn piece-locs
  [pieces]
  (if (string? pieces)
    (->> pieces
         (map vector str-ixs)
         (filter #(not= \. (peek %)))
         (map #(update % 0 str-ix->ix)))
    pieces))

(defn occupied-locs
  [pieces]
  (if (string? pieces)
    (->> (piece-locs pieces)
         (map first))
    (keys pieces)))

(defn replace-in-str [in f from len]
  (let [before (subs in 0 from)
        after (subs in (+ from len))
        being-replaced (subs in from (+ from len))
        replaced (f being-replaced)]
    (str before replaced after)))

(defn move-piece
  [pieces piece loc loc']
  (if (string? pieces)
    (let [p (loc->piece pieces loc)
          p' (loc->piece pieces loc')]
      (assert (= p piece))
      (assert (= p' \.))
      (-> pieces
          (replace-in-str (constantly p') (ix->str-ix loc) 1)
          (replace-in-str (constantly p) (ix->str-ix loc') 1)))
    (do
      (assert (= (get pieces loc) piece))
      (-> pieces
        (assoc loc' (get pieces loc))
        (dissoc loc)))))

(defn encode-pieces
  ([pieces]
   (encode-pieces pieces false))
  ([pieces encode?]
   (if encode?
     (->> str-ixs
          (map str-ix->ix)
          (map #(get pieces % \.))
          (apply str))
     pieces)))

(defn visualize-pieces
  [pieces]
  (let [hallway (apply str (map (comp loc->piece ->hallway) (range 11)))]
    (str hallway "\n"
         "  " (loc->piece pieces (->room \A 0))
         " "  (loc->piece pieces (->room \B 0))
         " "  (loc->piece pieces (->room \C 0))
         " "  (loc->piece pieces (->room \D 0)) "  \n"
         "  " (loc->piece pieces (->room \A 1))
         " "  (loc->piece pieces (->room \B 1))
         " "  (loc->piece pieces (->room \C 1))
         " "  (loc->piece pieces (->room \D 1)) "  \n"
         "  " (loc->piece pieces (->room \A 2))
         " "  (loc->piece pieces (->room \B 2))
         " "  (loc->piece pieces (->room \C 2))
         " "  (loc->piece pieces (->room \D 2)) "  \n"
         "  " (loc->piece pieces (->room \A 3))
         " "  (loc->piece pieces (->room \B 3))
         " "  (loc->piece pieces (->room \C 3))
         " "  (loc->piece pieces (->room \D 3)) "  \n")))

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
      (assoc-in [:pieces (->room \A 1)] \D)
      (assoc-in [:pieces (->room \B 1)] \C)
      (assoc-in [:pieces (->room \C 1)] \B)
      (assoc-in [:pieces (->room \D 1)] \A)
      (assoc-in [:pieces (->room \A 2)] \D)
      (assoc-in [:pieces (->room \B 2)] \B)
      (assoc-in [:pieces (->room \C 2)] \A)
      (assoc-in [:pieces (->room \D 2)] \C)
      (assoc-in [:pieces (->room \A 3)] (get-in input [:pieces (->room \A 1)]))
      (assoc-in [:pieces (->room \B 3)] (get-in input [:pieces (->room \B 1)]))
      (assoc-in [:pieces (->room \C 3)] (get-in input [:pieces (->room \C 1)]))
      (assoc-in [:pieces (->room \D 3)] (get-in input [:pieces (->room \D 1)]))))

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
                        (let [label (loc->label b)
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
      (map #(loc->piece pieces %))
      (remove nil?)
      set))

(defn path-open?
  [{:keys [path-between]} pieces a b]
  (let [path (-> (path-between a b)
                 (disj a))
        occupied (occupied-locs pieces)]
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
    (for [[loc p] (piece-locs pieces)
          :when (or (= (loc->type loc) :room)
                    (->> (pieces-in-room map-def pieces p)
                         (remove #(= p %))
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
      [(move-piece pieces p loc loc')
       (path-cost map-def p loc loc')])))

(defn estimate-cost
  [map-def pieces]
  (->> (piece-locs pieces)
       (map (fn [[loc label]]
              (* (piece-cost label)
                 (min (distance map-def (->room label 0) loc)
                      (distance map-def (->room label 1) loc)
                      (distance map-def (->room label 2) loc)
                      (distance map-def (->room label 3) loc)))))
       (reduce + 0)))

(defn setup-search
  [{:keys [map-def pieces]}]
  (search/setup
   {:initial-states [(encode-pieces pieces)]
    :neighbor-fn #(possible-moves map-def %)
    :est-fn #(estimate-cost map-def %)
    ;; :est-fn (constantly 0)
    :target (->> pieces
                 (map (comp #(vector % (loc->label %)) first))
                 (into {})
                 encode-pieces)}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        add-map-fns
        setup-search
        (iterate search/explore-next)
        (take 400000)
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
  (->> sample
       add-map-fns
       setup-search
       (iterate search/explore-next)
       (drop 4000)
       first
       :visited
       (group-by peek)
       (map #(vector (first %) (count (peek %))))
       sort)
  
  (->> sample
       add-map-fns
       :map-def
       :path-between
       (#(% (->room \A 1) (->room \C 0)))
       (map #(vector (loc->type %) (loc->label %) (loc->ix %)))
       sort)
  (sort [[0 1] [443 1] [3441 1] [240 3] [229 1] [20 4] [3460 2] [224 1] [463 2] [60 2] [27 1] [24 1] [260 2] [3464 1] [221 2] [464 1] [264 1] [50 1] [21 2] [460 2] [241 2] [3463 2] [420 1] [263 2] [40 6] [5460 1] [3440 3] [223 2] [3420 1] [41 4] [3474 1] [461 1] [243 3] [43 5] [29 1] [44 3] [5440 1] [465 1] [3444 1] [227 1] [220 3] [25 1] [261 1] [440 3] [444 1] [23 2] [230 1] [47 1] [3471 1] [200 1] [3473 2] [244 2] [441 1] [30 2] [3461 1] [3470 1] [3443 1] [49 1]])
  (sort [[0 1] [5480 1] [3484 1] [443 1] [3441 1] [240 3] [20 4] [3460 2] [224 1] [463 2] [27 1] [24 1] [260 1] [3464 1] [3510 1] [221 2] [464 1] [264 1] [21 2] [460 1] [3481 1] [241 2] [3463 2] [5490 1] [420 1] [263 2] [40 5] [5460 1] [3440 2] [223 2] [3511 1] [5511 1] [3420 1] [5484 1] [3490 1] [41 4] [3491 1] [461 1] [243 3] [5510 1] [43 5] [44 3] [5440 1] [5491 1] [465 1] [3444 1] [5470 1] [220 3] [25 1] [261 1] [440 2] [5481 1] [444 1] [23 2] [47 1] [3471 1] [200 1] [244 2] [441 1] [3480 1] [5483 1] [3461 1] [3470 2] [3483 1] [3443 1]])
  (print (visualize-pieces (encode-pieces (:pieces sample))))
  (update "12345" 3 (constantly \2))
  (occupied-locs (encode-pieces (:pieces (part-2-input sample))))
  (let [a {:1 3 :3 6} b {:3 2 :1 5}] (time (dotimes [_ 100000] (= a b))))
  (let [a [2 5] b [3 6]] (time (dotimes [_ 100000] (= a b))))
  (let [a "12345" b "23456"] (time (dotimes [_ 100000] (= a b))))
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8080)
  (prof/profile (time (do-1 sample)))
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
