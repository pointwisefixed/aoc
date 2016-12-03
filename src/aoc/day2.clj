(ns aoc.day2)

(require '[clojure.string :as str])

(defrecord Button [num left right up down])

(defn create-buttons [start end]
  (for [i (range start (+ end 1))] (Button. i (if (= 1 (mod i 3)) i (dec i)) (if (= 0 (mod i 3)) i (inc i)) (if (< (- i 3) 1) i (- i 3)) (if (> (+ i 3) 9) i (+ i 3)))))

(def keypad (create-buttons 1 9))

(defn find-button [but-num] (first (filter #(= (:num %) but-num) keypad)))

(defn press [but-num dir] (dir but-num))

(defn to-dir [line-moves] (map #(cond (= % \L) :left (= % \R) :right (= % \U) :up :else :down) line-moves))

(defn process-line [start-button line] (reduce #(find-button (%2 %1)) start-button (to-dir line)))

(defn solve-puzzle [list-of-line-moves]
  (str/join (map #(:num %) (drop 1 (reductions process-line (find-button 5) list-of-line-moves)))))

(defn list-of-moves [input] (into [] (with-open [rdr (clojure.java.io/reader input)] (doall (line-seq rdr)))))

(defn day2 [input-file] (let [code (solve-puzzle (list-of-moves input-file))] code))
