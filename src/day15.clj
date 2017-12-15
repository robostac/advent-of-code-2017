(ns day15)
(require '[clojure.string :as str])

(def aMult 16807)
(def bMult 48271)
(def modVal 2147483647)

(defn nextInSeq
  [n fact]
  (let [v (mod (* n fact) modVal)]
    (cons v (lazy-seq (nextInSeq v fact)))))

(defn cmpSeq
  [seqa seqb]
  (cons (= (bit-and (first seqa) 0xFFFF) (bit-and (first seqb) 0xFFFF))  (lazy-seq (cmpSeq (next seqa) (next seqb)))))

(defn day15a
  []
  (let [astart (Integer. (read-line))
        bstart (Integer. (read-line))]
    (println (count (filter true? (take 40000000 (cmpSeq (nextInSeq astart aMult) (nextInSeq bstart bMult))))))))

(defn nextInSeqb
  [n fact check]
  (let [v (mod (* n fact) modVal)]
    (if (= 0 (mod v check))
      (cons v (lazy-seq (nextInSeqb v fact check)))
      (recur v fact check))))

(defn day15b
  []
  (let [astart (Integer. (read-line))
        bstart (Integer. (read-line))]
    (println (count (filter true? (take 5000000 (cmpSeq (nextInSeqb astart aMult 4) (nextInSeqb bstart bMult 8))))))))
