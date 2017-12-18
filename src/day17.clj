(ns day17)

(defn spinLock
  [counter data curpos incval finish]
  (if (> counter finish)
    data
    (let [nextpos (+ 1 (mod (+ curpos incval) (count data)))]
      (recur (inc counter)  (concat (take nextpos data)  (list counter) (take-last (- (count data) nextpos) data)) nextpos incval finish))))

(defn day17a
  []
  (let [v (Integer. (read-line))]
    (println (spinLock 1 (list 0) 0 v 2017))))

(defn vAfterZero
  [counter curpos incval finish nval]
  (if (> counter finish)
    nval
    (let [nextpos (+ 1 (mod (+ curpos incval) counter))
          newval (if (= nextpos 1) counter nval)]
      (recur (inc counter) nextpos  incval finish newval))))

(defn day17b
  []
  (let [v (Integer. (read-line))]
    (println (vAfterZero 1 0  v 50000000 0))))