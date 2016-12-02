(ns aoc.core)
(require '[clojure.string :as str])

(defrecord Point [x y facing])

(defrecord Move [direction steps])

(defn move-to [currently move]
  (println currently)
  (println move)
  (let [currently-facing (:facing currently) current-x (:x currently) current-y (:y currently)
        direction (:direction move) steps (:steps move)]
    (if (= currently-facing 'North)
      (if (= direction "L") 
        (Point. (- current-x steps) current-y 'West)
        (Point. (+ current-x steps) current-y 'East)))
    (if (= currently-facing 'South)
      (if (= direction "L") 
        (Point. (+ current-x steps) current-y 'East)
        (Point. (- current-x steps) current-y 'West)))
    (if (= currently-facing 'East)
      (if (= direction "L") 
        (Point. current-x (+ current-y steps) 'North)
        (Point. current-x (- current-y steps) 'South)))
    (if (= currently-facing 'West)
      (if (= direction "L") 
        (Point. current-x (- current-y steps) 'South)
        (Point. current-x (+ current-y steps) 'North)))))

(defn to-move[mv-str] (let [[all dir steps] (flatten (re-seq #"([LR])(\d+)" mv-str))] (Move. dir (Integer/parseInt steps))))

(defn solve-day1-part1 [moves]
  (let [start (Point. 0 0 'North) 
        end (reduce move-to start moves)
        x-steps (Math/abs (- (:x end) (:x start)))
        y-steps (Math/abs (- (:y end) (:y start)))]
    (+ x-steps y-steps)))

(defn day1-part1
  "Day 1 Taxi Cab Driver"
  [input-file]
  (let [moves (map to-move (str/split (str/trim (slurp input-file)) #", "))]
    (solve-day1-part1 moves)))

;(day1-part1 "/home/pwfixed/Projects/aoc/resources/input")
