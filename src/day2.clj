(ns day2)
(require '[clojure.string :as str])

(def input (map #(str/split % #" ") (str/split-lines (slurp "../2.in"))))

(defn shape-score1
  "get the score for the shape you selected"
  [shape]
  (case shape
    "X" 1
    "Y" 2
    "Z" 3
    0)
  )
(defn shape-score2
  "get the score for the shape you will select depending on if you want to win"
  [a, b]
  (case b
    "X" (case a
          "A" 3
          "B" 1
          "C" 2)
    "Y" (case a
          "A" 1
          "B" 2
          "C" 3)
    "Z" (case a
          "A" 2
          "B" 3
          "C" 1)
    :else 0)
  )

(defn match-score1
  "get your score of the match, depending on who won"
  [a, b]
  (cond
    (and (= a "A") (= b "X")) 3
    (and (= a "B") (= b "Y")) 3
    (and (= a "C") (= b "Z")) 3
    (and (= a "C") (= b "X")) 6
    (and (= a "A") (= b "Y")) 6
    (and (= a "B") (= b "Z")) 6
    :else 0
    )
  )

(defn match-score2
  "get your score of the match, based on who is supposed to wini"
  [b]
  (case b
    "X" 0
    "Y" 3
    "Z" 6
    0)
  )

(prn (str "Part 1: " (reduce + (map (fn [inp] (+ (shape-score1 (second inp)) (match-score1 (first inp) (second inp)))) input))))
(prn (str "Part 2: " (reduce + (map (fn [inp] (+ (shape-score2 (first inp) (second inp)) (match-score2 (second inp)))) input))))

