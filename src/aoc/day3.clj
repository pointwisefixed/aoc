(ns aoc.day3)

(require '[clojure.string :as str])

(defrecord Result[part1 part2])

(defn valid? [[a b c]] (and (> (+ a b) c) (> (+ a c) b) (> (+ b c) a)))

(defn to-ints [vals] (into [] (map #(Integer/parseInt (str/trim %)) (str/split vals #"\s+"))))

(defn triangles-p1 [input] (into [] (map to-ints (map str/trim (str/split (slurp input) #"\n"))))) 

(defn triangles-p2 [input] (partition 3 (flatten (apply mapv vector (map #(map (fn[x] (Integer/parseInt x)) (re-seq #"\d{1,3}" %)) (str/split (slurp input) #"\n"))))))

(defn day3 [input-file] (let [possible-triangles-p1 (triangles-p1 input-file)
                              possible-triangles-p2 (triangles-p2 input-file)]
  (Result. (apply + (map #(if (valid? %) 1 0) possible-triangles-p1))
           (apply + (map #(if (valid? %) 1 0) possible-triangles-p2)))))
