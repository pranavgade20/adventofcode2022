(ns day6)
(require '[clojure.string :as str])

(def input (first (str/split-lines (slurp "../6.in"))))
(def len (count input))

(defn get-subset [coll n m] (doall (take-last (- m n) (take m coll))))

(prn (str "Part 1: " (first (filter (fn [i]
       ; return if i-4 to i is a subseq with no duplicates
       (= 4 (count (set (get-subset input (- i 4) i))))
       ) (range 4 len)))))

(prn (str "Part 2: " (first (filter (fn [i]
       ; return if i-14 to i is a subseq with no duplicates
       (= 14 (count (set (get-subset input (- i 14) i))))
       ) (range 14 len)))))
