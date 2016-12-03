(ns aoc.day1-test
  (:require [clojure.test :refer :all]
            [aoc.day1 :refer :all]))

(deftest day1-test
  (testing "Testing AOC Day1"
    (let [result (day1 "./resources/day1/input")]
      (is (= 181 (:manhattan-dist result)))
      (is (= 140 (:first-visited-twice result))))))
