(ns advent-of-code-2021.day17
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.set :as set]))

(defn parse-input
  [input]
  (->> input
       (re-find #"target area: x=(.*)\.\.(.*), y=(.*)\.\.(.*)")
       rest
       (map #(Integer/parseInt %))
       (zipmap [:xmin :xmax :ymin :ymax])))

(def sample (parse-input "target area: x=20..30, y=-10..-5"))
(def input (parse-input (core/get-input 17)))

(defn tri
  [x]
  (/ (* x (inc x)) 2))

(defn max-vy
  [input]
  ;; The projectile flies up and back to zero in a perfect parabola.
   ;; The speed at the bottom is the same as the one it launched with.
   ;; It will overshoot the target if the initial speed is higher than
   ;; the lowest y value for the target.
   (-> input
       :ymin
       (* -1)
       dec))

(defn min-vy
  [input]
  ;; Any faster downward, and the projectile will overshoot.
  (-> input
      :ymin))

(defn max-vx
  [input]
  ;; Any faster forward, and the projectile will overshoot.
  (-> input
      :xmax))

(defn min-vx
  [input]
  ;; The projectile travels a triangle-number distance forward before
  ;; stalling due to drag. square-root of 2x slightly underestimates
  ;; the initial velocity needed to hit x.
  (-> input
      :xmin
      (* 2)
      Math/sqrt
      Math/floor))

(defn time-in-target-y
  [{:keys [ymin ymax]} vy]
  (set (for [t (range 1000)
             :let [y (- (* t vy) (tri (dec t)))]
             :when (and (<= ymin y)
                        (<= y ymax))]
         t)))

(defn time-in-target-x
  [{:keys [xmin xmax]} vx]
  (set (for [t (range 1000)
                  :let [x (- (tri vx) (tri (max 0 (- vx t))))]
                  :when (and (<= xmin x)
                             (<= x xmax))]
              t)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   ;; The height reached is the triangle number for the initial velocity.
   ;; The height reached is the triangle number for the initial velocity.
   (-> input
       max-vy
       tri)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (let [[min-vx max-vx min-vy max-vy] ((juxt min-vx max-vx min-vy max-vy) input)
         txs (->> (range min-vx (inc max-vx))
                  (mapv #(vector % (time-in-target-x input %))))
         tys (->> (range min-vy (inc max-vy))
                  (mapv #(vector % (time-in-target-y input %))))
         hit-vectors (for [[vx tx] txs
                           [vy ty] tys
                           :when (seq (set/intersection tx ty))]
                           [vx vy])]
     (count hit-vectors))))

(comment
  (seq (set/intersection (time-in-target-x sample 17)
                    (time-in-target-y sample -4)))
  sample
  (do-2 sample)
  (sort (map (partial apply vector) (partition 2 [23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
   25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
   8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
   26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
   20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
   25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
   25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
   8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
   24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
   7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
   23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
   27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
   8,-2    27,-8   30,-5   24,-7])))
  (tri 2)
  (set [1 2 3])
  (time-in-target-x input 15))

