(ns day10)
(require '[clojure.string :as str])

(defn crossHatch
  [vec start sublength length]
  (let [nv (into [] (concat vec vec))
        revpart (reverse (subvec nv start (+ start sublength)))
        rest (subvec nv (+ start sublength) (+ start length))]
    (let [res (into [] (concat revpart rest))
          part (mod (- length start) (count vec))]
      (concat (subvec res part) (subvec res 0 part)))))

(defn calchash
  [lst inp start skip]
  ;;(println  lst inp start skip)
  (if inp
    (recur (crossHatch lst start (first inp) (count lst)) (next inp) (mod (+ start (+ skip (first inp))) (count lst)) (mod (inc skip) (count lst)))
    [lst start skip]))

(defn day10a
  []
  (let [y (map #(Integer. %) (str/split (read-line) #","))]
    (println (calchash (range 256) y 0 0))))

(defn fullHash
  [lst inp count start skip]
  (if (> count 0)
    (let [res (calchash lst inp start skip)]
      (recur (first res) inp (dec count) (nth res 1) (nth res 2)))
    lst))

(defn denseHash
  [lst b]
  (if (> (count lst) 0)
    (let [val (reduce bit-xor (subvec (into [] lst) 0 16))]
      (recur  (subvec (into [] lst) 16) (conj b val)))
    b))

(defn hexify
  [s]
  (apply str
         (map #(format "%02x" (int %)) (flatten s))))

(defn knotHash
  [in]
  (let [y (concat (map #(int %) (into [] in)) [17 31 73 47 23])]
    (denseHash (fullHash (range 256) y 64 0 0) [])))

(defn day10b
  []
  (println (hexify (knotHash (read-line)))))
    ;(println y)))
