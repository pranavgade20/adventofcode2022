(ns day7)
(require '[clojure.string :as str])

(def input (concat (str/split-lines (slurp "../7.in")) (repeatedly 100 (fn [] "$ cd .."))))


(defn get-chunk-header
  "get next chunk header, i.e., things before the $"
  [ip]
  (take (first (filter (fn [i] (str/starts-with? (nth ip i) "$")) (range))) ip)
  )

(defn parse-item
  "parse one line of a chunk, either a file or a folder"
  [item]
  (let [split-item (str/split item #" ")]
    (if (= "dir" (first split-item))
      {}
      (Integer/parseInt (first split-item)))
    )
  )

(defn parse-chunk-header
  "take a chunk, and return a dict describing the contents"
  [ip]
  (zipmap (map (fn [x] (second (str/split x #" "))) ip) (map (fn [x] (parse-item x)) ip))
  )

(defn parse-tree
  "parse the tree of directories"
  [inp, start-index]
  (loop [i (+ start-index (count (get-chunk-header (drop start-index inp)))), dict (parse-chunk-header (get-chunk-header (drop start-index inp)))]
    (if (= "$ cd .." (nth inp i))
      [i, dict]
      (let [command (nth inp i), dir-name (nth (str/split command #" ") 2), i' (+ i 2)]
        (if (str/starts-with? command "$ cd")
          (let [res (parse-tree inp, i')] (recur (inc (first res)) (update dict dir-name (fn [prev] (second res)))))
          (println "error " command))))
    ))

(def tree (second (parse-tree input 2)))

(defn traverse-tree
  "traverse the tree"
  [root]
  (if (not (map? root))
    [0, root]
    (let [mapped (map #(traverse-tree %) (vals root)), cnt (apply + (map #(first %) mapped)), sz (apply + (map #(second %) mapped))] [(+ (if (> sz 100000) 0 sz) cnt), sz])))

(prn (str "Part 1: " (first (traverse-tree tree))))
;(prn (traverse-tree tree))