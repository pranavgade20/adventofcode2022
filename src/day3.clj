(ns day3)
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (str/split-lines (slurp "../3.in")))

(prn (str "Part 1: " (reduce + (map (fn [i] (cond (>= i (int \a)) (+ 1 (- i (int \a))) :else (+ 27 (- i (int \A)))))(map (fn [s] (int(first (set/intersection (first s) (second s))))) (map #(list (set (first %)) (set (second %))) (map (fn [i] (split-at (/ (count i) 2) i)) input)))))))
(prn (str "Part 2: " (reduce + (map (fn [i] (cond (>= i (int \a)) (+ 1 (- i (int \a))) :else (+ 27 (- i (int \A)))))(map (fn [s] (int(first (set/intersection (first s) (second s) (nth s 2))))) (partition 3 (map #(set %) input)))))))
