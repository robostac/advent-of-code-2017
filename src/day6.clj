(ns day6)
(require '[clojure.string :as str])

(defn distribute
  [current items pos]
  (if (= items 0)
    current
    (let [nextpos (mod pos (count current))
          v (nth current nextpos)]
      (recur (assoc current nextpos (inc v)) (dec items) (inc nextpos)))))

(defn nextSol
  [current]
  (let [mv (reduce max current)
        ind (.indexOf current mv)]
    (distribute (assoc current ind 0) (nth current ind) (inc ind))))

(defn findSol
  [count visited current]
  (if (contains? visited current)
    [count current]
    (recur (inc count) (conj visited current) (nextSol current))))

(defn day6a
  []
  (let [y (map #(Integer. %) (str/split (read-line) #"\s"))
        z (into [] y)]
    (println (first (findSol 0 (set z) z)))))

(defn day6b
  []
  (let [y (map #(Integer. %) (str/split (read-line) #"\s"))
        z (into [] y)
        init (second (findSol 0 (set z) z))]
    (println (first (findSol 0 (set init) init)))))
