(ns day1)
(require '[clojure.string :as str])
(defn third
  "get the third element in a list"
  [list]
  (nth list 2))
(def input (str/split-lines (slurp "../1.in")))

(def partitioned (filter #(not= "" (first %)) (partition-by (fn [i] (= i "")) input)))

(def sorted (sort > (map (fn [l] (reduce + (map (fn [i] (Integer/parseInt i)) l))) partitioned)))

(prn (str "Part 1: " (first sorted)))
(prn (str "Part 2: " (+ (first sorted) (second sorted) (third sorted))))

