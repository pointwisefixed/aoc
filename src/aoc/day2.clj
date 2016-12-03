(ns aoc.day2)

(require '[clojure.string :as str])

(defrecord Button [num left right up down])

(defrecord Result [part1 part2])

(defn create-buttons-part1 [start end]
  (for [i (range start (+ end 1))] (Button. (str i) (str (if (= 1 (mod i 3)) i (dec i))) (str (if (= 0 (mod i 3)) i (inc i))) (str (if (< (- i 3) 1) i (- i 3))) (str (if (> (+ i 3) 9) i (+ i 3))))))

(def keypad-part1 (create-buttons-part1 1 9))

(def keypad-part2 
  (list (Button. "1" "1" "1" "1" "3") (Button. "2" "2" "3" "2" "6") (Button. "3" "2" "4" "1" "7") (Button. "4" "3" "4" "4" "8") 
        (Button. "5" "5" "6" "5" "5") (Button. "6" "5" "7" "2" "A") (Button. "7" "6" "8" "3" "B") (Button. "8" "7" "9" "4" "C") 
        (Button. "9" "8" "9" "9" "9") (Button. "A" "A" "B" "6" "A") (Button. "B" "A" "C" "7" "D") (Button. "C" "B" "C" "8" "C") (Button. "D" "D" "D" "B" "D")))

(defn find-button [but-num keypad] (first (filter #(= (:num %) but-num) keypad)))

(defn press [but-num dir] (dir but-num))

(defn to-dir [line-moves] (map #(cond (= % \L) :left (= % \R) :right (= % \U) :up :else :down) line-moves))

(defn process-line [start-button line keypad] (reduce #(find-button (%2 %1) keypad) start-button (to-dir line)))

(defn solve-puzzle [list-of-line-moves keypad]
  (str/join (map #(:num %) (drop 1 (reductions #(process-line %1 %2 keypad) (find-button "5" keypad) list-of-line-moves)))))

(defn list-of-moves [input] (into [] (with-open [rdr (clojure.java.io/reader input)] (doall (line-seq rdr)))))

(defn day2 [input-file] (let [moves (list-of-moves input-file)]
                          (Result. (solve-puzzle moves keypad-part1) (solve-puzzle moves keypad-part2))))
