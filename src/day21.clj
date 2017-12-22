(ns day21)
(require '[clojure.string :as str])
(def initSquare
  [".#."
   "..#"
   "###"])

(defn flip2
  [ptn]
  [(nth ptn 2) (nth ptn 3) (nth ptn 0) (nth ptn 1)])

(defn flip3
  [ptn]
  [(nth ptn 6) (nth ptn 7) (nth ptn 8)
   (nth ptn 3) (nth ptn 4) (nth ptn 5)
   (nth ptn 0) (nth ptn 1) (nth ptn 2)])

(defn rot2
  [ptn]
  []
  [(nth ptn 2) (nth ptn 0) (nth ptn 3) (nth ptn 1)])

(defn rot3
  [ptn]
  [(nth ptn 6) (nth ptn 3) (nth ptn 0)
   (nth ptn 7) (nth ptn 4) (nth ptn 1)
   (nth ptn 8) (nth ptn 5) (nth ptn 2)])

(defn flip
  [ptn]
  (case (count ptn)
    4 (flip2 ptn)
    9 (flip3 ptn)))

(defn rot
  [ptn]
  (case (count ptn)
    4 (rot2 ptn)
    9 (rot3 ptn)))

(defn addPattern
  [list in2 out]
  (let [in (into [] in2)
        fin (flip in)]
    (assoc list
           in out
           fin out
           (rot in) out
           (rot fin) out
           (rot (rot in)) out
           (rot (rot fin)) out
           (rot (rot (rot in))) out
           (rot  (rot (rot fin))) out)))

(defn convertSquare
  [x y size graph rules]
  (let [endp (+ x size)
        ptn (str
             (subs (nth graph y) x endp)
             (subs (nth graph (+ y 1)) x endp)
             (if (= size 3) (subs (nth graph (+ y 2)) x endp)))]

    (get rules (into [] ptn))))

(defn extractPattern
  [in]
  (let [spl (str/split in #" ")
        match  (str/replace (first spl) #"/" "")
        opt (str/split (nth spl 2) #"/")]
    [match opt]))

(defn buildRules
  [ptns list]
  (if ptns
    (recur (next ptns) (addPattern list (first (first ptns)) (second (first ptns))))
    list))

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))

(defn buildRow
  [y graph rules]
  (let [size (if (= (mod (count graph) 2) 0) 2 3)]
    (map str/join (partition (quot (count graph) size) (apply interleave (map #(convertSquare % y size graph rules) (range 0 (count graph) size)))))))

(defn applyRules
  [graph rules]
  (let [size (if (= (mod (count graph) 2) 0) 2 3)]
    (mapcat #(buildRow % graph rules) (range 0 (count graph) size))))

(defn applyRulesMult
  [count graph rules]
  (if (= count 0)
    graph
    (recur (dec count) (applyRules graph rules) rules)))

(defn day21a
  []
  (let [y (map extractPattern (line-seq (java.io.BufferedReader. *in*)))
        rules (buildRules y {})
        v (applyRulesMult 5 initSquare rules)]
    (println (count (filter #(= % \#) (flatten  (map #(into [] %) v)))))))

(defn day21b
  []
  (let [y (map extractPattern (line-seq (java.io.BufferedReader. *in*)))
        rules (buildRules y {})
        v (applyRulesMult 18 initSquare rules)]
    (println (count (filter #(= % \#) (flatten  (map #(into [] %) v)))))))