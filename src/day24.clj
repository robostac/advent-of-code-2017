(ns day24)
(require '[clojure.string :as str])

(defrecord pipe [a b val])
(defrecord fullpipe [length val])

(defn extractPipe
  [line]
  (let [y (str/split line #"/")]
    (->pipe
     (Integer. (first y))
     (Integer. (second y))
     (+ (Integer. (first y)) (Integer. (second y))))))

(defn findPipes
  [avail end]
  (filter some?
          (map #(if (= (:a (second %)) end)
                  [(:b (second %)) (:val (second %)) (first %)]
                  (if (= (:b (second %)) end)
                    [(:a (second %)) (:val (second %)) (first %)])) avail)))

(defn removePipe
  [poss avail]
  [(dissoc avail (nth poss 2))
   (first poss)
   (second poss)])

(defn finishPipe
  [avail end curpipe]
  (let [poss (findPipes avail end)]
    (if (> (count poss) 0)
      (let [fp (map #(removePipe % avail) poss)]
        (flatten (map #(finishPipe (first %) (second %) (conj curpipe (nth % 2))) fp)))
      (->fullpipe (count curpipe) (reduce + curpipe)))))

(defn day24a
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))
        p (zipmap (range) (map extractPipe y))]
    (println p)
    (println (reduce max (map :val (finishPipe p 0 []))))))

(defn day24b
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))
        p (zipmap (range) (map extractPipe y))
        pipes (finishPipe p 0 [])
        maxlen (reduce max (map :length pipes))]
    (println (reduce max (map :val  (filter #(= (:length %) maxlen) pipes))))))