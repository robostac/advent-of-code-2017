(ns day5)
(require '[clojure.string :as str])

(defn inc5b
  [val]
  (if (>= val 3)
    (dec val)
    (inc val)))

(defn jmp
  [current table count nextinc]
  (let [cj (nth table current nil)]
    (if (nil? cj)
      count
      (recur (+ current cj) (assoc table current (nextinc cj)) (+ count 1) nextinc))))

(defn day5a
  []
  (let [input (into [] (map #(Integer. %) (line-seq (java.io.BufferedReader. *in*))))]
    (println (jmp 0 input 0 inc))))

(defn day5b
  []
  (let [input (into [] (map #(Integer. %) (line-seq (java.io.BufferedReader. *in*))))]
    (println (jmp 0 input 0 inc5b))))