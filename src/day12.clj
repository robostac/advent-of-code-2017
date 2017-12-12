(ns day12)
(require '[clojure.string :as str])

(defstruct prog12 :id :links)

(defn getProg12
  [in]
  (let [spl (str/split in #"\s" 3)]
    (struct prog12 (first spl) (map str/trim (str/split (nth spl 2) #",")))))

(defn buildGraph12
  [input graph]
  (if input
    (let [p (getProg12 (first input))]
      (recur (next input) (assoc graph (:id p) p)))
    graph))

(defn addLinks
  [links nodes visited]
  (if links
    (if (contains? visited (first links))
      (recur (next links) nodes visited)
      (recur (next links) (conj nodes (first links)) (conj visited (first links))))
    [nodes visited]))

(defn buildLinks
  [graph nodes visited]
  (if nodes
    (let [al (addLinks (:links (get graph (first nodes))) (next nodes) visited)]
      (recur graph (first al) (second al)))
    visited))

(defn day12a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildGraph12 y (hash-map))]
    (println (count (buildLinks tree ["0"] (hash-set))))))

(defn countGroups
  [graph keys visited groups]
  (if keys
    (if (contains? visited (first keys))
      (recur graph (next keys) visited groups)
      (recur graph (next keys) (buildLinks graph [(first keys)] visited) (inc groups)))
    groups))

(defn day12b
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildGraph12 y (hash-map))]
    (println (countGroups tree (keys tree) (hash-set) 0))))
