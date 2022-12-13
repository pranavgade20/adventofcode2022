(ns day10)
(require '[clojure.string :as str])

(def input (->> "../10.in"
                (slurp)
                (str/split-lines)
                ))

(defn get-states
  "get states based on instruction"
  [a, instruction]
  (case instruction
    "noop" [a]
    [a, (+ a (Integer/parseInt (.substring instruction 5)))]
    ))


(->> (loop [i 0 states [1]]
      (if (< i (count input))
        (recur
          (inc i)
          (concat states (get-states (last states) (nth input i))))
        states))
     (concat (repeat 20 -1))
     (partition 40)
     (map last)
     (take 6)
     (map vector (range 20 (+ 20 (* 40 6)) 40))
     (map (partial apply *))
     (apply +)
     (str "Part 1: ")
     (prn))

(defn abs [x]
  (max x (- x)))

(->> (loop [i 0 states [1]]
       (if (< i (count input))
         (recur
           (inc i)
           (concat states (get-states (last states) (nth input i))))
         states))
     (map vector (flatten (repeat (range 40))))
     (map (fn [[x y]] (if (> 2 (abs (- x y))) \# \.)))
     (partition 40)
     (map (partial apply str))
     (run! println))