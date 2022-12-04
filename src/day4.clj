(ns day4)
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "../4.in")))

(def ranges (map (fn [l]
                   (map
                     (fn [i] (Integer/parseInt i)) l))
                 (map (fn [s]
                        (str/split s #"[,-]"))
                      input)))

(prn (str "Part 1: " (reduce + (map (fn [l]
                                      (let [a (nth l 0) b (nth l 1) c (nth l 2) d (nth l 3)]
                                        (cond
                                          (and (<= a c) (>= b d)) 1
                                          (and (<= c a) (>= d b)) 1
                                          :else 0
                                          )))
                                    ranges))))
(prn (str "Part 2: " (reduce + (map (fn [l]
                                      (let [a (nth l 0) b (nth l 1) c (nth l 2) d (nth l 3)]
                                        (cond
                                          (< b c) 0
                                          (< d a) 0
                                          :else 1
                                          )))
                                    ranges))))
