(ns day2 (:gen-class))
(require '[clojure.string :as str])

(defn checksum
  [line]
  (let [y (map #(Integer. %) (str/split line #"\t"))]
    (- (reduce max y) (reduce min y))))

(defn checkdiv
  [nums,div]
  (let [d (filter #(= (mod % div) 0)
                  (filter #(not= % div) nums))]
    (if (> (count d) 0)
      (/ (first d) div)
      0)))

(defn checksumdiv
  [line]
  (let [y (map #(Integer. %) (str/split line #"\t"))]
    (reduce + (map #(checkdiv y %) y))))

(defn day2a
  []
  (println (reduce + (map
                      checksum
                      (line-seq (java.io.BufferedReader. *in*))))))

(defn day2b
  []
  (println (reduce + (map
                      checksumdiv
                      (line-seq (java.io.BufferedReader. *in*))))))