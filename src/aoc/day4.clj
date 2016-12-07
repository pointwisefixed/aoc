(ns aoc.day4)

(require '[clojure.string :as str])

(defrecord Result[part1 part2])

(def matching-pattern #"([a-z-]*)-(\d*)\[(\w*)\]")

(defn is-real-room?[room-id] 
  (let [[ignore letters id checksum] (flatten (re-seq matching-pattern room-id))
         real-checksum  (str/join "" (take 5 (map first (apply concat (map sort (map second (group-by second (reverse (sort-by second (frequencies (filter #(not (= \- %)) letters)))))))))))]
  [(Integer/parseInt id) (= checksum real-checksum)]))

(defn read-input[input] (str/split (slurp input) #"\n"))

(defn day4 [input-file] 
  (let [room-ids (read-input input-file)
        valid-room-ids (map first (filter #(= true (second %)) (map is-real-room? room-ids)))]
    (Result. (apply + valid-room-ids) nil)))
