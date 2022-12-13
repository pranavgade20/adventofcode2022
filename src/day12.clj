(ns day12)
(require '[clojure.string :as str])

(def input (->> "../12.in"
                (slurp)
                (str/split-lines)
                (map (fn [x] x))
                (map vec)
                (vec)
                ))

(def width (count (first input)))
(def height (count input))

(defn get-elevation
  "get the elevation at a point"
  [[x y] space]
  (nth (nth space y) x))

(defn find-symbol [symbol space] (loop [y 0]
                             (let [x (loop [x 0]
                                       (cond
                                         (= width x) -1
                                         (= (get-elevation [x y] space) symbol) x
                                         :else (recur (inc x))))]
                               (cond
                                 (= y height) -1
                                 (< -1 x) [x y]
                                 :else (recur (inc y)))
                               )))
(def start (find-symbol \S input))
(def end (find-symbol \E input))

(def input (assoc-in input [(second start) (first start)] \a))
(def input (assoc-in input [(second end) (first end)] \z))


(defn all-neighbours
  "get all the neighnours without filtering"
  [[x y]]
  (->> '([-1 0] [1 0] [0 -1] [0 1])
       (map (fn [p] (mapv + p [x y])))
       (filter (fn [[x y]] (and (< -1 x width) (< -1 y height))))
       )
  )

(defn get-neighbours
  "get filtered neighbours"
  [[x y] space visited]
  (->> (all-neighbours [x y])
       (filter (fn [[x' y']] (> 2 (- (int (get-elevation [x' y'] space)) (int (get-elevation [x y] space))))))
       (filter (fn [[x' y']] (= [-1 -1] (nth (nth visited y') x'))))
       ))

(defn bfs
  "breadth-first search the space"
  [space start end]
  (loop [stack [start] visited (vec (repeat height (vec (repeat width [-1 -1])))) ]
    (let [curr (first stack)
          neighbours (get-neighbours curr space visited)
          visited' (reduce (fn [visited' [x y]] (assoc-in visited' [y x] curr)) visited neighbours)]
      (cond
        (= end curr) visited'
        (= 0 (count stack)) [-1 -1]
        :else (recur (concat (drop 1 stack) neighbours) visited')))))

(def paths (bfs input start end))

(prn (str "Part 1: " (count (loop [path '() curr end]
  (if (= curr start)
    path
    (recur (concat path [curr]) (nth (nth paths (second curr)) (first curr))))))))


(defn get-neighbours
  "get filtered neighbours"
  [[x y] space visited]
  (->> (all-neighbours [x y])
       (filter (fn [[x' y']] (< -2 (- (int (get-elevation [x' y'] space)) (int (get-elevation [x y] space))))))
       (filter (fn [[x' y']] (= [-1 -1] (nth (nth visited y') x'))))
       ))


(defn bfs
  "breadth-first search the space"
  [space start]
  (loop [stack [start] visited (assoc-in (vec (repeat height (vec (repeat width [-1 -1])))) [(second start) (first start)] [-10 -10])]
    (let [curr (first stack)
          neighbours (get-neighbours curr space visited)
          visited' (reduce (fn [visited' [x y]] (assoc-in visited' [y x] curr)) visited neighbours)]
      (cond
        (= (nth (nth space (second curr)) (first curr)) \a) [curr visited']
        (= 0 (count stack)) [-1 -1]
        :else (recur (concat (drop 1 stack) neighbours) visited')))))

(def search-res (bfs input end))
(def paths (second search-res))
(def start end)
(def end (first search-res))

(prn (str "Part 2: " (count (loop [path '() curr end]
       (if (= curr start)
         path
         (recur (concat path [curr]) (nth (nth paths (second curr)) (first curr))))))))

