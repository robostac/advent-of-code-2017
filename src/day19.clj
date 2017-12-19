(ns day19)
(require '[clojure.string :as str])

(defrecord point19 [x y])

(defn addPoints
  [p1 p2]
  (->point19 (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2))))

(defn getNext19
  [p]
  [p
   (->point19 (- (:y p)) (:x p))
   (->point19 (:y p) (- (:x p)))])

(defn addRow19
  [row data index col]
  (if row
    (recur (next row) (assoc data (->point19 col index) (first row)) index (inc col))
    data))

(defn buildMap19
  [lines data index]
  (if lines
    (recur (next lines) (addRow19 (into [] (first lines)) data index 0) (inc index))
    data))

(defn findStart
  [data]
  (first (first (filter #(and (=  (:y (first %)) 0) (not= (second %) \space)) data))))

(defn getVal19
  [graph pos dir]
  (let [np (addPoints pos dir)
        nv (get graph np)]
    [np nv dir (if (re-matches #"[A-Z]" (str nv)) nv nil)]))

(defn follow
  [graph pos dir build count]
  (let [possible (map #(getVal19 graph pos %) (getNext19 dir))
        actual (first (filter #(not= (second %) \space) possible))]
    (if actual
      (recur graph (first actual) (nth actual 2) (str build (nth actual 3)) (inc count))
      [build count])))

(defn day19a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        graph (buildMap19 y {} 0)
        start (findStart graph)]
    (println (follow graph start (->point19 0 1) "" 1))))

(defn day19b
  []
  (day19a))
