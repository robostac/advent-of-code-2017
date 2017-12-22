(ns day22)
(require '[clojure.string :as str])

(defrecord point22 [x y])

(defn addPoints22
  [p1 p2]
  (->point22 (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2))))

(defn getNext22
  [p inf]
  (case inf
    \# (->point22 (- (:y p)) (:x p))
    \. (->point22 (:y p) (- (:x p)))
    \w p
    \f (->point22 (- (:x p)) (- (:y p)))))

(defn applyVirus22a
  [inf]
  (case inf
    \# \.
    \. \#))

(defn addRow22
  [row data index col]
  (if row
    (recur (next row) (assoc data (->point22 col index) (first row)) index (inc col))
    data))

(defn buildMap22
  [lines data index]
  (if lines
    (recur (next lines) (addRow22 (first lines) data index 0) (inc index))
    data))

(defn follow22
  [moves graph cp dir infected nextFn]
  (if (= moves 0)
    infected
    (let [curp (get graph cp \.)
          nextd (getNext22 dir curp)
          nexti (nextFn curp)
          nextinf (if (= nexti \#) (inc infected) infected)]
      (recur (dec moves) (assoc graph cp nexti) (addPoints22 cp nextd) nextd nextinf nextFn))))

(defn day22a
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))
        graph (buildMap22 y {} 0)
        si (quot (count y) 2)
        sp (->point22 si si)
        sd (->point22 0 -1)]
    (println (follow22 10000 graph sp sd 0 applyVirus22a))))

(defn applyVirus22b
  [inf]
  (case inf
    \# \f
    \. \w
    \w \#
    \f \.))

(defn day22b
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))
        graph (buildMap22 y {} 0)
        si (quot (count y) 2)
        sp (->point22 si si)
        sd (->point22 0 -1)]
    (println (follow22 10000000 graph sp sd 0 applyVirus22b))))