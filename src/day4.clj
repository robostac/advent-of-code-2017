(ns day4)
(require '[clojure.string :as str])

(defn checkUnique
  [line]
  (let [y (str/split line #" ")]
    (if (= (count y) (count (distinct y)))
      1
      0)))

(defn checkUniqueAna
  [line]
  (let [y (map sort (str/split line #" "))]
    (if (= (count y) (count (distinct y)))
      1
      0)))

(defn day4a
  []
  (println (reduce + (map
                      checkUnique
                      (line-seq (java.io.BufferedReader. *in*))))))

(defn day4b
  []
  (println (reduce + (map
                      checkUniqueAna
                      (line-seq (java.io.BufferedReader. *in*))))))