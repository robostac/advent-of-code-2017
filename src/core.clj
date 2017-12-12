(ns core
  (:gen-class)
  (:use day1)
  (:use day2)
  (:use day3)
  (:use day4)
  (:use day5)
  (:use day6)
  (:use day7)
  (:use day8)
  (:use day9)
  (:use day10)
  (:use day11)
  (:use day12))

(defn -main [& args]
  (println (nth args 1))
  (case (nth args 1)
    "1a" (day1a)
    "1b" (day1b)
    "2a" (day2a)
    "2b" (day2b)
    "3a" (day3a)
    "3b" (day3b)
    "4a" (day4a)
    "4b" (day4b)
    "5a" (day5a)
    "5b" (day5b)
    "6a" (day6a)
    "6b" (day6b)
    "7a" (day7a)
    "7b" (day7b)
    "8a" (day8a)
    "8b" (day8b)
    "9a" (day9a)
    "9b" (day9b)
    "10a" (day10a)
    "10b" (day10b)
    "11a" (day11a)
    "11b" (day11b)
    "12a" (day12a)
    "12b" (day12b)))
