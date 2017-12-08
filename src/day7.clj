(ns day7)
(require '[clojure.string :as str])

(defstruct program :name :weight :links)

(defn getProgram
  [in]
  (let [spl (str/split in #"\s" 4)]
    (if (> (count spl) 2)
      (struct program (first spl) (Integer. (str/replace (second spl) #"[\(\)]" "")) (map str/trim (str/split (nth spl 3) #",")))
      (struct program (first spl) (Integer. (str/replace (second spl) #"[\(\)]" "")) []))))

(defn hasLink
  [p key]
  (if (> (count (filter #(= key %) (:links p))) 0)
    1
    0))

(defn countLinks
  [key tree]
  (reduce + (map #(hasLink % key) (vals tree))))

(defn buildTree
  [in tree]
  (if in
    (let [p (getProgram (first in))]
      (recur (next in) (assoc tree (:name p) p)))
    tree))

(defn weigtSum
  [key tree]
  (let [vals (map #(weigtSum % tree) (:links (get tree key)))]

    (if (> (count (distinct vals)) 1)
      (println (:links (get tree key)) vals (map #(:weight (get tree %)) (:links (get tree key)))))
    (+ (:weight (get tree key)) (reduce + vals))))

(defn day7a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildTree y (hash-map))]
        ;;(println (buildTree y (hash-map )))
    (println (filter #(= (countLinks % tree) 0) (keys tree)))))

(defn day7b
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildTree y (hash-map))
        root (first (filter #(= (countLinks % tree) 0) (keys tree)))]
    (println (map #(weigtSum % tree) (:links (get tree root))))))

