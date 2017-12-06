(ns day1 (:gen-class))
(require '[clojure.string :as str])

(defn value [input, step, start]
  (if (< start (count input))
    (let [c (nth input start)
          n (nth input (mod (+ start step) (count input)))]
      (let [match (if (= c n)
                    (Character/digit c 10)
                    0)]

        (+ (value input step (+ start 1))  match)))
    0))

(defn day1a
  []
  (let [x (read-line)]
    (println (value x 1 0))))

(defn day1b
  []
  (let [x (read-line)]
    (println (value x (/ (count x) 2) 0))))
