(ns day13)
(require '[clojure.string :as str])

(defn parseInput
  [in]
  (let [spl (map #(Integer. %) (map str/trim (str/split in #"\:")))]
    [(first spl) (second spl) (- 2 (* 2 (second spl)))]))

(defn getCaught
  [in offset]
  (= 0 (mod (+ offset (first in)) (nth in 2))))

(defn getVal13
  [in]
  (let [spl (parseInput in)]
    (if (getCaught spl 0)
      (* (first spl)  (second spl))
      0)))

(defn day13a
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))]
    (println (reduce + (map getVal13 y)))))

(defn findTime
  [in time]
  (let [val (some true? (map #(getCaught % time) in))]
    (if val
      (recur in (inc time))
      time)))

(defn day13b
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))]
    (println (findTime (map parseInput y) 0))))
