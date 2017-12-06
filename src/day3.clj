(ns day3)

(def gridWidth 1000)
(def midPoint (/ gridWidth 2))

(def nextDir (hash-map
              1 (- 0 gridWidth)
              (- 0 gridWidth) -1
              -1 gridWidth
              gridWidth 1))

(defn getXY
  [point]
  [(mod point gridWidth) (quot point gridWidth)])

(defn getPoint
  [x y]
  (+ (* y  gridWidth) x))

(def startPoint (getPoint midPoint midPoint))

(defn getValue
  [spiral point]
  (def nextPoints [(getPoint 1 1) (getPoint 1 0) (getPoint 0 1) (getPoint -1 1) (getPoint -1 -1) (getPoint 1 -1) (getPoint -1 0) (getPoint 0 -1)])
  (reduce + (map #(get spiral (+ point %) 0) nextPoints)))

(defn finish3a
  [count target value]
  (if (= count target)
    true
    false))

(defn finish3b
  [count target value]
  (if (> value target)
    true
    false))

(defn buildSpiral
  [count current  curdir spiral target finish]
  (let [next (get nextDir curdir)
        value (getValue spiral current)
        np (if (contains? spiral (+ current next)) curdir next)
        newspiral (assoc spiral current value)]
    (if (finish count target value)
      [current newspiral]
      (recur (inc count) (+ current  np) np newspiral target finish))))

(defn day3a
  []
  (let [x (Integer. (read-line))]
    (let [y (buildSpiral 2 (inc startPoint) 1 (hash-map startPoint, 0) x finish3a)]
      (println (reduce + (map #(Math/abs (- midPoint %)) (getXY (first y))))))))

(defn day3b
  []
  (let [x (Integer. (read-line))]
    (let [y (buildSpiral 2 (inc startPoint) 1 (hash-map startPoint, 1) x finish3b)]
      (println (get (second y) (first y))))))