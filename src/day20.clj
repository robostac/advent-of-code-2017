(ns day20)
(require '[clojure.string :as str])

(defrecord point20 [x y z])
(defrecord particle20 [p v a])

(defn parsePoint
  [in]
  (let [matcher (re-matcher #".*<\s?(-?\d+),(-?\d+),(-?\d+)>.*" in)
        data  (re-find matcher)]
    (apply ->point20 (map #(Integer. %) (next data)))))

(defn parseParticle
  [in]
  (let [partspl  (str/split in #", " 3)]
    [(apply ->particle20 (map parsePoint partspl))]))

(defn manhattan
  [point]
  (reduce + (map #(Math/abs %) (vals point))))

(defn addpoints
  [p1 p2]
  (apply ->point20 (map + (vals p1)  (vals p2))))

(defn advance
  [particle]
  (let [newv (addpoints (:v particle) (:a particle))
        newp (addpoints (:p particle) newv)]
    (->particle20 newp newv (:a particle))))

(defn advanceTurns
  [count parts]
  (if (= count 0)
    parts
    (recur (dec count) (advance parts))))

(defn day20a
  []
  (let [graph (mapcat parseParticle (line-seq (java.io.BufferedReader. *in*)))
        mina (filter #(= 1 (manhattan (:a %))) graph)
        mm (apply min-key #(manhattan (:p (advanceTurns 500 %))) mina)]
    (println (.indexOf graph mm))))

(defn removeCollisions
  [graph]
  (let [freq (frequencies (map :p graph))]
    (filter #(= 1 (get freq (:p %))) graph)))

(defn collisonTurns
  [graph turns]
  (if (= 0 turns)
    graph
    (recur (removeCollisions (map advance graph)) (dec turns))))

(defn day20b
  []
  (let [graph (mapcat parseParticle (line-seq (java.io.BufferedReader. *in*)))
        ct (collisonTurns graph 100)]
    (println (count ct))))
