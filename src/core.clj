(ns core
  (:gen-class)
  (:use day1)
  (:use day2)
  (:use day3)
  (:use day4)
  (:use day5)
  (:use day6)
  (:use day7)
  (:use day8))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

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
    "8b" (day8b)))
