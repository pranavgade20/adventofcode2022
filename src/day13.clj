(ns day13)
(require '[clojure.string :as str])

(def input (->> (-> "../13.in"
                    (slurp)
                    (str/split #"\n\n"))
                (map str/split-lines)
                (map (fn [[x y]] [(eval (read-string x)) (eval (read-string y))])))
  )

(defn get-ret
  "get the return val"
  [x]
  (case x
    0 ))

(defn compare-exprs
  "compare two elems"
  [x y]
   (cond
    (and (integer? x) (integer? y)) (cond (= x y) 0 :else (if (< x y) 1 2))
    (and (= [] x) (= [] y)) 0
    (= [] x) 1
    (= [] y) 2
    (and (vector? x) (vector? y)) (let [ret (compare-exprs (first x) (first y))] (if (= 0 ret) (compare-exprs (vec (rest x)) (vec (rest y))) ret))
    (integer? x) (compare-exprs [x] y)
    (integer? y) (compare-exprs x [y])
    ))

(defn compare-exprs-wrapper
  "wrapper to return true/false using compare-exprs"
  [x y]
  (= 1 (compare-exprs x y)))


(println (str "Part 1: " (->> (range (count input))
          (filter (fn [x] (= 1 (compare-exprs (first (nth input x)) (second (nth input x))))))
          (map (partial + 1))
          (apply +))))

(def input-2  (concat (->> "../13.in"
                   (slurp)
                   (str/split-lines)
                   (filter (fn [x] (not (str/blank? x))))
                   (map (fn [x] (eval (read-string x))))) [[[2]],[[6]]])
  )

(println (str "Part 2: " (let [sorted (sort compare-exprs-wrapper input-2)] (->> (range (count input-2))
                                                                 (filter (fn [x] (or (= (nth sorted x) [[2]]) (= (nth sorted x) [[6]]))))
                                                                 (map (partial + 1))
                                                                 (apply *)
                                                                 ))))

