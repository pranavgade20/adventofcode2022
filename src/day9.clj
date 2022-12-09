(ns day9)
(require '[clojure.string :as str])

(def input (->> "../9.in"
                (slurp)
                (str/split-lines)
                (map (fn [^String st] [(. st charAt 0) (Integer/parseInt (. st substring 2))]))
                ;(map #(repeat (second %) (first %)))
                ;(flatten)
                ;(map {\R [1 0] \L [-1 0] \U [0 1] \D [0 -1]})
                ))

(def input-len (count input))
(def moves {\R [1 0] \L [-1 0] \U [0 1] \D [0 -1]})

(defn move-t
  "moves t to correct pos, based on h and t position"
  [[hx hy] [tx ty]]
  (let [dx (- hx tx), dy (- hy ty), del (+ (* dx dx) (* dy dy))]
    (if (< del 3)
      [tx ty]
      (cond
        (= 0 dx) [tx, (+ ty (if (neg? dy) -1 1))]
        (= 0 dy) [(+ tx (if (neg? dx) -1 1)), ty]
        :else [(+ tx (if (neg? dx) -1 1)), (+ ty (if (neg? dy) -1 1))]
        ))))

(defn move-rope
  "move the rope of 9 tails"
  [head tails]
  (loop [i 0 h head ts tails]
    (if (= i 9)
      ts
      (let [i' (inc i) h' (nth ts i) t' (move-t h h')]
        (recur i' t' (assoc ts i t'))))))

(defn apply-move
  "apply a move sequence"
  [head tail [dir n]]
  (loop [i 0 h head t tail ret []]
    (if (= i n)
      [h t ret]
      (let [i' (inc i) h' (mapv + h (moves dir)) t' (move-t h t)]
        (recur i' h' t' (concat ret [t']))))))
(defn apply-move-rope
  "apply a move sequence"
  [head tails [dir n]]
  (loop [i 0 h head ts tails ret []]
    (if (= i n)
      [h ts ret]
      (let [i' (inc i) h' (mapv + h (moves dir)) ts' (move-rope h' ts)]
        (recur i' h' ts' (concat ret [(last ts')]))))))

(prn (str "Part 1: " (count (set (loop [i 0 h [0 0] t [0 0] ret [[0 0]]]
       (if (= i input-len)
         ret
         (let [i' (inc i) [h' t' ret'] (apply-move h t (nth input i))]
           (recur i' h' t' (concat ret ret')))))))))


(prn (str "Part 2: " (count (set (loop [i 0 h [0 0] ts (vec (repeat 9 [0 0])) ret [[0 0]]]
       (if (= i input-len)
         ret
         (let [i' (inc i) [h' t' ret'] (apply-move-rope h ts (nth input i))]
           (recur i' h' t' (concat ret ret')))))))))

