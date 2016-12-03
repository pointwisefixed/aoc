(ns aoc.core)
(require '[clojure.string :as str])

(defrecord Coord [x y])

(defrecord Point [x y facing visited first-twice-visited])

(defrecord Move [direction steps])

(defrecord Result [manhattan-dist first-visited-twice])

(defn fix-range [a b] (cond (> a b) (map inc (reverse (range b a))) :else (range a b)))

(defn path [p1 p2] (let [x1 (:x p1) y1 (:y p1) x2 (:x p2) y2 (:y p2)] (cond (= x1 x2) (map (fn[v] (Coord. x1 v)) (fix-range y1 y2)) (= y1 y2) (map (fn[v] (Coord. v y1)) (fix-range x1 x2)))))

(defn move-to [{:keys [x y facing visited first-twice-visited]} {:keys [direction steps]}]
  (let [[new-x new-y new-facing] 
        (cond
          (or (and (= facing 'North) (= direction "L")) (and (= facing 'South) (= direction "R"))) [(- x steps) y 'West]
          (or (and (= facing 'North) (= direction "R")) (and (= facing 'South) (= direction "L"))) [(+ x steps) y 'East]
          (or (and (= facing 'East) (= direction "L")) (and (= facing 'West) (= direction "R"))) [x (+ y steps) 'North]
          (or (and (= facing 'East) (= direction "R")) (and (= facing 'West) (= direction "L"))) [x (- y steps) 'South])
        new-coord (Coord. new-x new-y)
        new-path (path (Coord. x y) new-coord)
        dup (if first-twice-visited first-twice-visited (first (filter (fn[v] (some #{v} visited)) new-path)))
        new-visited (into [] (concat visited new-path))]
    (Point. new-x new-y new-facing new-visited dup)))

(defn calc-dist[end start] (+ (Math/abs (- (:x end) (:x start))) (Math/abs (- (:y end) (:y start)))))

(defn to-move[mv-str] (let [[all dir steps] (flatten (re-seq #"([LR])(\d+)" mv-str))] (Move. dir (Integer/parseInt steps))))

(defn solve-day1-part1 [moves] (let [start (Point. 0 0 'North [] nil) end (reduce move-to start moves) 
                                     first-visited-twice (:first-twice-visited end)]
    (Result. (calc-dist end start) (if first-visited-twice (calc-dist first-visited-twice start)))))

(defn day1 [input-file] (let [moves (map to-move (str/split (str/trim (slurp input-file)) #", "))] (solve-day1-part1 moves)))
