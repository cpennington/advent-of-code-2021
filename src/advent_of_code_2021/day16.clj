(ns advent-of-code-2021.day16
  (:require
   [advent-of-code-2021.core :as core]))

(def bits {\0 [0 0 0 0]
           \1 [0 0 0 1]
           \2 [0 0 1 0]
           \3 [0 0 1 1]
           \4 [0 1 0 0]
           \5 [0 1 0 1]
           \6 [0 1 1 0]
           \7 [0 1 1 1]
           \8 [1 0 0 0]
           \9 [1 0 0 1]
           \A [1 0 1 0]
           \B [1 0 1 1]
           \C [1 1 0 0]
           \D [1 1 0 1]
           \E [1 1 1 0]
           \F [1 1 1 1]})

(defn bitstream
  [input]
  {:bits (->> input
              (map bits)
              (apply concat))
   :pos 0})

(defn read
  [bs n]
  {:post [(= (-> % first count) n)]}
  [(take n (:bits bs))
   (-> bs
       (update :bits #(drop n %))
       (update :pos + n))])

(defn split
  [bs n]
  (let [[fst rest] (read bs n)]
    [{:bits fst :pos (:pos bs)} rest]))

(defn bs-empty?
  [bs]
  (-> bs :bits empty?))

(defn bits->int
  [bits]
  (Long/parseLong (apply str bits) 2))

(declare read-packet)

(defn read-packets
  [bs]
  (iterate (fn [[packets bs]]
             (let [[packet bs] (read-packet bs)]
               [(conj packets packet) bs]))
           [[] bs]))

(defn read-literal
  [bs]
  (let [[_ content bs] (->>  [true [] bs]
                             (iterate (fn [[_ bytes bs]]
                                        (let [[[det & bytes*] bs] (read bs 5)]
                                          [(= det 1) (into bytes bytes*) bs])))
                             (drop-while first)
                             first)]
    [(bits->int content) bs]))

(defn read-operator-length
  [bs]
  (let [[length-bits bs] (read bs 15)
        length (bits->int length-bits)
        [sub-packet-bs bs] (split bs length)
        [contents _] (->> (read-packets sub-packet-bs)
                          (drop 1)
                          (drop-while #(-> % peek bs-empty? not))
                          first)]
    [contents bs]))

(defn read-operator-count
  [bs]
  (let [[count-bits bs] (read bs 11)
        count (bits->int count-bits)
        [contents bs] (->> (read-packets bs)
                           (drop count)
                           first)]
    [contents bs]))

(defn read-operator
  [bs]
  (let [[[length-type] bs] (read bs 1)]
    (case length-type
      0 (read-operator-length bs)
      1 (read-operator-count bs))))

(defn read-packet
  [bs]
  (let [[v-bits bs] (read bs 3)
        version (bits->int v-bits)
        [t-bits bs] (read bs 3)
        type (bits->int t-bits)
        [content bs] (case type
                       4 (read-literal bs)
                       (read-operator bs))]
    [{:version version
      :type type
      :contents content} bs]))

(defn eval-packet
  [{:keys [version type contents]}]
  (if (number? contents)
    contents
    (let [content-vals (map eval-packet contents)
          fn (case type
               0 +
               1 *
               2 min
               3 max
               5 (comp {true 1 false 0} >)
               6 (comp {true 1 false 0} <)
               7 (comp {true 1 false 0} =))]
      (apply fn content-vals))))

(defn sum-versions
  [{:keys [version contents]}]
  (if (number? contents)
    version
    (reduce + version (map sum-versions contents))))

(def sample (bitstream "A0016C880162017C3686B18A3D4780"))
(def input (bitstream (core/get-input 16)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (-> (read-packet input)
       first
       sum-versions)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (-> input
       read-packet
       first
       eval-packet)))

(comment
  (-> "C200B40A82"
      bitstream
      read-packet
      first
      eval-packet)
  (eval-packet (read-packet (bitstream "C200B40A82")))
  (do-1 sample)
  (read-packet (bitstream "D2FE28"))
  (read-packet (bitstream "38006F45291200"))
  (read-packet (bitstream "EE00D40C823060"))
  (-> (bitstream "ABC")
      (update :bits #(drop 3 %))
      (update :pos + 3))
  (Long/parseLong (apply str [\1 \0 \1]) 2)
  )

