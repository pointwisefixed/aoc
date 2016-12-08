(ns aoc.day5)

(require '[clojure.string :as str])

(import 'java.security.MessageDigest
                'java.math.BigInteger)

(defrecord Result[part1 part2])

(defn md5 [s]
    (let [algorithm (MessageDigest/getInstance "MD5")
                  size (* 2 (.getDigestLength algorithm))
                          raw (.digest algorithm (.getBytes s))
                                  sig (.toString (BigInteger. 1 raw) 16)
                                          padding (apply str (repeat (- size (count sig)) "0"))]
          (str padding sig)))

(defn filtered-hashes[pwd]
 (filter #(.startsWith % "00000") (pmap #(md5 (str pwd %)) (range))))

(defn is-number-and-less-than-8?[hash] (and (Character/isDigit (nth hash 5)) (< (Integer/parseInt (str (nth hash 5))) 8)))

(defn new-filter[pwd] (filter is-number-and-less-than-8? (filtered-hashes pwd)))

(defn to-vec[hash](let[index (Integer/parseInt (str (nth hash 5)))] (vector index (.charAt hash 6)))) 

(defn to-code[pwd code] (if (nth pwd (first code)) pwd (assoc pwd (first code) (second code))))

(defn decode2[enc-pwd] (str/join "" (loop [pwd (into [] (repeat 8 nil)) codes (map to-vec (new-filter enc-pwd))] (if (and (some nil? pwd) (not (empty? codes))) (recur (to-code pwd (first codes)) (rest codes)) pwd))))

(defn decode[pwd]
   (str/join "" (map #(nth % 5) (take 8 (filtered-hashes pwd)))))

(defn day5[input-file]
  (let [enc-pwd (.replace (slurp input-file) "\n" "")
        pwd (decode enc-pwd)
        pwd2 (decode2 enc-pwd)]
    (Result. pwd pwd2)))
