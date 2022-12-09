(ns day8)
(require '[clojure.string :as str])

(def input (map (fn [l] (map #(Integer/parseInt (str %)) l))(str/split-lines (slurp "../8.in"))))

(defn rotate
  "rotate a 2d array"
  [arr]
  ; 1 2 3
  ; 4 5 6
  (map (fn [i]
         (map #(nth (nth arr %) i) (range (count arr))))
       (reverse (range (count (first arr)))))
  )

(defn rotate2
  "rotate twice"
  [arr]
  (rotate (rotate arr)))

(defn rotate3
  "rotate thrice"
  [arr]
  (rotate (rotate (rotate arr))))

(defn visible-left
  "get if visible from left"
  [arr]
  (map (fn [l] 
         (loop [i 0, curr-max -1, ret []]
           (if (= i (count l))
             ret
             (let [i' (inc i), visible (< curr-max (nth l i)), new-max (max (nth l i) curr-max)]
               (recur i' new-max (concat ret [visible]))
               )
             )
           )
         )
       arr)
  )
(prn (str "Part 1: " (apply + (let [a (-> input (visible-left) (flatten)), b (-> input (rotate) (visible-left) (rotate3) (flatten)), c (-> input (rotate2) (visible-left) (rotate2) (flatten)), d (-> input (rotate3) (visible-left) (rotate) (flatten))]
  (map (fn [pred] (if pred 1 0))(map (fn [w,x,y,z] (or w x y z)) a b c d))
  ))))

(defn scenic-score-left
  "get scenic score of tree looking to the left"
  [arr]
  (map (fn [l]
         (map (fn [k] (loop [i k, ret 0]
           (if (or (>= i (dec (count l))))
             ret
             (if (>= (nth l (inc i)) (nth l k))
               (recur (count l) (inc ret))
               (recur (inc i) (inc ret)))

             )
           )) (range (count l)))
         )
       arr)
  )

(prn (str "Part 2: " (apply max (let [a (-> input (scenic-score-left) (flatten)), b (-> input (rotate) (scenic-score-left) (rotate3) (flatten)), c (-> input (rotate2) (scenic-score-left) (rotate2) (flatten)), d (-> input (rotate3) (scenic-score-left) (rotate) (flatten))]
                                (map (fn [w,x,y,z] (* w x y z)) a b c d)
                                ))))
