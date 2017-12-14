(ns day14
  (:use day10))

(require '[clojure.string :as str])

(def bits (hash-map
           \0 [0 0 0 0]
           \1 [0 0 0 1]
           \2 [0 0 1 0]
           \3 [0 0 1 1]
           \4 [0 1 0 0]
           \5 [0 1 0 1]
           \6 [0 1 1 0]
           \7 [0 1 1 1]
           \8 [1 0 0 0]
           \9 [1 0 0 1]
           \a [1 0 1 0]
           \b [1 0 1 1]
           \c [1 1 0 0]
           \d [1 1 0 1]
           \e [1 1 1 0]
           \f [1 1 1 1]))

(defn blockHash
  [in x]
  (hexify (knotHash (str in "-" x))))

(defn bitsSet
  [kn]
  (reduce + (map #(reduce + (get bits %)) kn)))

(defn addRow
  [data row grid col]
  (if data
    (recur (next data) row (assoc grid (+ row col) (first data)) (inc col))
    grid))

(defn addGrid
  [kn grid row]
  (let [data  (reduce concat (map #(get bits %) kn))]
    (addRow data (* row 130) grid 0)))

(defn buildGrid
  [knl grid row]
  (if knl
    (recur (next knl) (addGrid (first knl) grid row) (inc row))
    grid))

(defn fillGrid
  [grid point]
  (if (not= (get grid point 0) 1)
    grid
    (let [updgrid (assoc grid point 2)
          updgrid1 (fillGrid updgrid (+ point 1))
          updgrid2 (fillGrid updgrid1 (+ point -1))
          updgrid3 (fillGrid updgrid2 (+ point -130))
          updgrid4 (fillGrid updgrid3 (+ point 130))]
      updgrid4)))

(defn countGroups14
  [grid point counter]
  (if (< point 0)
    counter
    (if (= 1 (get grid point 0))
      (recur (fillGrid grid point) (dec point) (inc counter))
      (recur grid (dec point) counter))))

(defn day14a
  []
  (let [in (read-line)]
    (println (reduce + (vals (buildGrid (map #(blockHash in %) (range 128)) (hash-map) 0))))))

(defn day14b
  []
  (let [in (read-line)]
    (println (countGroups14 (buildGrid (map #(blockHash in %) (range 128)) (hash-map) 0) (* 130 130) 0))))
