(ns aoc.day6)

(require '[clojure.string :as str])

(defrecord Result[part1 part2])

(defn read-input[input-file] (str/split (slurp input-file) #"\n"))

(defn solve[input-file fn]
  (let [lines (read-input input-file)
       part1 (str/join "" (map first (map #(first (fn (sort-by val (frequencies %)))) (apply mapv vector (into [] (map #(str/split % #"") lines))))))]
    part1))

(defn day6 [input-file] 
  (let[pt1 (solve input-file reverse)
       pt2 (solve input-file identity)] (Result. pt1 pt2)))
