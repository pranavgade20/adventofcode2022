(ns day5)
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "../5.in")))

(defn get-substr
  "get chars representing crates"
  [s, i]
  (cond
   (>= i (count s)) ""
   :else (str (nth s i) (get-substr s (+ i 4)))
   ))

(def crates (reverse (map (fn [s] (get-substr s 1))
                          (filter (fn [s] (str/starts-with? (str/trim s) "[")) input))))

(defn get-stacks
  "get the ith stack of crates"
  [l i]
  (map (fn [s] (nth s i)) l))

(def stk (map (fn [i] (str/trim (apply str (get-stacks crates i)))) (range (- (reduce max (map #(count %) crates)) 0))))


(def instructions (map (fn [l]
                         [(Integer/parseInt (nth l 1)),(- (Integer/parseInt (nth l 2)) 1),(- (Integer/parseInt (nth l 3)) 1)])
                       (map (fn [s]

                                    (str/split s #"[a-z ]+")
                                    )
                       (filter (fn [s]
                                 (str/starts-with? s "move")
                                 )
                               input)))
  )

(defn get-subset [coll n m] (doall (take-last (- m n) (take m coll))))

(defn apply-instructions-1
  "applies given instruction to the stacks"
  [instruction stacks]
  (let [a (nth instruction 0) b (nth instruction 1) c (nth instruction 2)]
    (concat
      (get-subset stacks 0 b)
      [(get-subset (nth stacks b) 0 (- (count (nth stacks b)) a))]
      (get-subset stacks (+ b 1) c)
      [(concat (nth stacks c) (reverse (take-last a (nth stacks b))))]
      (get-subset stacks (+ c 2) (+ (count stacks) 1))
      )
    )
  )
(defn apply-instructions-2
  "applies given instruction to the stacks"
  [instruction stacks]
  (let [a (nth instruction 0) c (nth instruction 1) b (nth instruction 2)]
    (concat
      (get-subset stacks 0 b)
      [(concat (nth stacks b) (reverse (take-last a (nth stacks c))))]
      (get-subset stacks (+ b 1) c)
      [(get-subset (nth stacks c) 0 (- (count (nth stacks c)) a))]
      (get-subset stacks (+ c 2) (+ (count stacks) 1))
      )
    )
  )


(def functions-to-transduce (doall (map (fn [instruction] (let [a (nth instruction 0) b (nth instruction 1) c (nth instruction 2)]
                                                            (if (< b c)
                                                              (partial apply-instructions-1 instruction)
                                                              (partial apply-instructions-2 instruction)
                                                              )
                                                            )) instructions)))

(prn (str "Part 1: " (apply str (map (fn [col] (last col))((apply comp (reverse functions-to-transduce)) stk)))))

(defn apply-instructions-1
  "applies given instruction to the stacks"
  [instruction stacks]
  (let [a (nth instruction 0) b (nth instruction 1) c (nth instruction 2)]
    (concat
      (get-subset stacks 0 b)
      [(get-subset (nth stacks b) 0 (- (count (nth stacks b)) a))]
      (get-subset stacks (+ b 1) c)
      [(concat (nth stacks c)  (take-last a (nth stacks b)))]
      (get-subset stacks (+ c 2) (+ (count stacks) 1))
      )
    )
  )
(defn apply-instructions-2
  "applies given instruction to the stacks"
  [instruction stacks]
  (let [a (nth instruction 0) c (nth instruction 1) b (nth instruction 2)]
    (concat
      (get-subset stacks 0 b)
      [(concat (nth stacks b)  (take-last a (nth stacks c)))]
      (get-subset stacks (+ b 1) c)
      [(get-subset (nth stacks c) 0 (- (count (nth stacks c)) a))]
      (get-subset stacks (+ c 2) (+ (count stacks) 1))
      )
    )
  )


(def functions-to-transduce (doall (map (fn [instruction] (let [a (nth instruction 0) b (nth instruction 1) c (nth instruction 2)]
                                                            (if (< b c)
                                                              (partial apply-instructions-1 instruction)
                                                              (partial apply-instructions-2 instruction)
                                                              )
                                                            )) instructions)))

(prn (str "Part 2: " (apply str (map (fn [col] (last col))((apply comp (reverse functions-to-transduce)) stk)))))

