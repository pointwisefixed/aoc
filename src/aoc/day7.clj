(ns aoc.day7)

(require '[clojure.string :as str])

(defn read-input[input-file] (str/split (slurp input-file) #"\n"))

(defn read-ip[ip]
  (filter #(not (nil? %)) (flatten (map #(vector (second %) (nth % 2)) (re-seq #"(\w+)?(\[\w+\])?" ip)))))

(defn group-ip[ip fn] (filter fn (read-ip ip)))

(defn is-abba?[txt]
  (loop [text txt found false](let [[a b c d] text] (cond found true (empty? text) false :else (recur (str/join "" (rest text)) (and (= a d) (= b c) (not (= a b))))))))

(defn process-ip[ip]
  (let [hypernet (group-ip ip (fn[v](.startsWith v "[")))
        supernet (group-ip ip (fn[v] (not (.startsWith v "["))))]
  [(map (fn[v] (str/join "" (filter #(and (not (= \] %)) (not (= \[ %))) v))) hypernet) supernet]))
 
(defn is-tls?[ip]
  (let [[hypernets supernets] (process-ip ip)]
    (and (reduce #(or %1 %2) (map is-abba? supernets))
         (reduce #(and %1 %2) (map #(not (is-abba? %)) hypernets)))))

(defn aba-parts[supernet] 
  (loop [abas []
         txt supernet]
    (let [[a b c] txt
          new-abas (if (and (= a c) (not (= a b))) (conj abas (str/join "" [a b c])) abas)]
    (cond (empty? txt) abas
    :else (recur new-abas (rest txt))))))

(defn flip-aba[aba]
  (let [[a b c] aba] (str b a b)))

(defn has-any-aba-in-bab?[hypernet abas]
  (let [ssnet (map flip-aba abas)]
    (not (empty? (filter (fn[x] (reduce #(or %1 %2) (or (map (fn[y] (.contains x y)) ssnet)))) hypernet)))))

(defn is-ssl?[ip]
  (let [[hypernets supernets] (process-ip ip)
        abas (flatten (filter #(not (empty? %)) (map aba-parts supernets)))]
    (cond (empty? abas) false :else (has-any-aba-in-bab? hypernets abas))))
 
(defrecord Results[part1 part2])

(defn sum-if-pred[pred lines]
  (apply + (map #(if (pred %) 1 0) lines)))

(defn day7[input-file]
  (let[lines (read-input input-file)
       pt1 (sum-if-pred is-tls? lines)
       pt2 (sum-if-pred is-ssl? lines)]
    (Results. pt1 pt2)))
