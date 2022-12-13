(ns day11)
(require '[clojure.string :as str])

(def input (-> "../11.in"
                (slurp)
                (str/split #"\n\n")
                ))

(defstruct Monkey :id :items :operation :test :if-true :if-false :inspected)

(defn parse-eqn
  "parse and return a function that can be applied"
  [eqn]
  (let [[op, b] (str/split eqn #" ")]
    (if (= b "old")
      (case op
        "+" (fn [x] (+ x x))
        "*" (fn [x] (* x x)))
      (case op
        "+" (fn [x] (+ (Integer/parseInt b) x))
        "*" (fn [x] (* (Integer/parseInt b) x)))
      )
    ))

(defn monkey-parser
  "parses data of a monkey, returns a struct"
  [in]
   (let [lines (str/split-lines in)]
     (struct Monkey
             (Integer/parseInt (second (re-seq #"Monkey |[0-9]+|:" (first lines))))
             (map (fn [x] (Integer/parseInt x)) (str/split (second(re-seq #"  Starting items: |[0-9, ]+" (nth lines 1))) #", "))
             (parse-eqn (second (re-seq #"  Operation: new = old |[\w\D]+" (nth lines 2))))
             (Integer/parseInt (second (re-seq #"  Test: divisible by |[\w]+" (nth lines 3))))
             (Integer/parseInt (second (re-seq #"    If true: throw to monkey |[\w]+" (nth lines 4))))
             (Integer/parseInt (second (re-seq #"    If false: throw to monkey |[\w]+" (nth lines 5))))
             0
             )))

(def monkeys (vec (map monkey-parser input)))
(def prod-tests (apply * (map (fn [m] (m :test)) monkeys)))

(defn monkey-turn
  "a monkey takes his turn"
  [monkeys, idx]
  (let [curr (nth monkeys idx), items (curr :items), monkeys (assoc-in (assoc-in monkeys [idx :items] '()) [idx :inspected] (+ (count items) (curr :inspected))), ]
    (loop [items items, monkeys monkeys]
      (if (= 0 (count items))
        monkeys
        (let [item (first items),
              worry-level (quot ((curr :operation) item) 3),
              throw-to (if (= 0 (mod worry-level (curr :test))) (curr :if-true) (curr :if-false)),
              monkeys' (assoc-in monkeys [throw-to :items] (concat [worry-level] ((nth monkeys throw-to) :items)))]
          (recur (drop 1 items), monkeys'))))))

(defn round
  "run one round of the game"
  [monkeys]
  (loop [monkeys monkeys, idx 0]
    (if (= idx (count monkeys))
      monkeys
      (recur (monkey-turn monkeys idx) (inc idx)))))

(def round-20
  (reduce (fn [x y] (y x)) monkeys (repeat 20 round)))

(prn (str "Part 1: " (apply * (take-last 2 (sort (map (fn [m] (m :inspected)) round-20))))))

(defn monkey-turn
  "a monkey takes his turn"
  [monkeys, idx]
  (let [curr (nth monkeys idx), items (curr :items), monkeys (assoc-in (assoc-in monkeys [idx :items] '()) [idx :inspected] (+ (count items) (curr :inspected))), ]
    (loop [items items, monkeys monkeys]
      (if (= 0 (count items))
        monkeys
        (let [item (first items),
              worry-level (mod ((curr :operation) item) prod-tests),
              throw-to (if (= 0 (mod worry-level (curr :test))) (curr :if-true) (curr :if-false)),
              monkeys' (assoc-in monkeys [throw-to :items] (concat [worry-level] ((nth monkeys throw-to) :items)))]
          (recur (drop 1 items), monkeys'))))))

(defn round
  "run one round of the game"
  [monkeys]
  (loop [monkeys monkeys, idx 0]
    (if (= idx (count monkeys))
      monkeys
      (recur (monkey-turn monkeys idx) (inc idx)))))

(def round-10000
  (reduce (fn [x y] (y x)) monkeys (repeat 10000 round)))

(prn (str "Part 2: " (apply * (take-last 2 (sort (map (fn [m] (m :inspected)) round-10000))))))
