(ns aoc.day2-test
  (:require [clojure.test :refer :all]
            [aoc.day2 :refer :all]))

(deftest day2-test
  (testing "Testing AOC Day2"
    (let [{:keys [part1 part2]} (day2 "./resources/day2/input")]
      (is (= part1 "61529"))
      (is (= part2 "C2C28")))))
