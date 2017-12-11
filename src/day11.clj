(ns day11)
(require '[clojure.string :as str])

(defstruct point :x :y :z)

(def nextMap (hash-map
              "ne" (struct point 1 0 -1)
              "se" (struct point 1 -1 0)
              "s"  (struct point 0 -1 1)
              "sw" (struct point -1 0 1)
              "nw" (struct point -1 1 0)
              "n" (struct point 0 1 -1)))

(defn addPoint
  [p1 p2]
  (struct point (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2)) (+ (:z p1) (:z p2))))

(defn getNext
  [curp next]
  (println curp)
  (addPoint curp (get nextMap next)))

(defn distance
  [curp]
  ;(abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)) / 2
  (/ (+ (Math/abs (:x curp)) (+ (Math/abs (:y curp)) (Math/abs (:z curp)))) 2))

(defn navigate
  [curp route mv]
  (if route
    (let [np (getNext curp (first route))
          dis (distance np)]
      (recur np (next route) (max mv dis)))
    [curp mv]))

(defn day11a
  []
  (let [y (str/split (read-line) #",")]
    (println (distance (first (navigate (struct point 0 0 0) y 0))))))

(defn day11b
  []
  (let [y (str/split (read-line) #",")]
    (println (second (navigate (struct point 0 0 0) y 0)))))
