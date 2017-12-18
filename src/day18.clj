(ns day18)
(require '[clojure.string :as str])

(defstruct inst18 :inst :x :y)

(defn instsnd
  [regs a b tgt]
  (assoc regs
         "snd" a
         "sndcnt" (inc (get regs "sndcnt" 0))))

(defn instset
  [regs a b tgt]
  (assoc regs
         tgt b))

(defn instadd
  [regs a b tgt]
  (assoc regs tgt  (+ a b)))

(defn instmul
  [regs a b tgt]
  (assoc regs
         tgt (* a b)))

(defn instmod
  [regs a b tgt]
  (assoc regs
         tgt (mod a b)))

(defn instrcv
  [regs a b tgt]
  (if (= a 0)
    regs
    (assoc regs "rcv" (get regs "snd"))))

(defn instjgz
  [regs a b tgt]
  (if (<= a 0)
    regs
    (assoc regs
           "pc" (+ (get regs "pc" 0) (- b 1)))))

(def instmapa
  {"add" instadd
   "snd" instsnd
   "rcv" instrcv
   "jgz" instjgz
   "mod" instmod
   "mul" instmul
   "set" instset})

(defn as-int
  [s regs]
  (try
    (Integer. s)
    (catch NumberFormatException e
      (get regs s 0))))

(defn getInst18
  [in]
  (let [spl (str/split in #"\s")]
    (if (= (count spl) 2)
      (struct inst18 (first spl) (nth spl 1) 0)
      (struct inst18 (first spl) (nth spl 1) (nth spl 2)))))

(defn buildInstList18
  [in tree]
  (if in
    (let [p (getInst18 (first in))]
      (recur (next in) (conj tree p)))
    tree))

(defn updateReg18
  [initregs inst instmap]
  (let [regs (assoc initregs "pc" (inc (get initregs "pc" 0)))]
    (let [v1 (as-int (:x inst) regs)
          v2 (as-int (:y inst) regs)]
      ((get instmap (:inst inst)) regs v1 v2 (:x inst)))))

(defn runInstList18
  [instlist regs]
  (if (contains? regs "rcv")
    (get regs "rcv")
    (let [newreg (updateReg18 regs (nth instlist (get regs "pc")) instmapa)]
      (runInstList18 instlist newreg))))

(defn day18a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildInstList18 y [])]
    (println (runInstList18  tree (hash-map "pc" 0)))))

(defn rcv
  [regs a b store]
  (let [mq (get regs "mq")]
    (if mq
      (assoc  regs
              store (first mq)
              "mq" (next mq)
              "hung" 0)
      (assoc regs
             "hung" 1
             "pc" (- (get regs "pc") 1)))))

(def instmapb (assoc instmapa "rcv" rcv))

(defn passMessages
  [regs regs2]
  (if (contains? regs "snd")
    [(dissoc regs "snd") (assoc regs2 "mq" (concat (get regs2 "mq") (list (get regs "snd"))))]
    [regs regs2]))

(defn runInstList18b
  [instlist initregs initregs2]
  (let [[ir1 ir2] (passMessages initregs initregs2)
        [regs2 regs] (passMessages ir2 ir1)]
    (let [newreg (updateReg18 regs (nth instlist (get regs "pc")) instmapb)
          newreg2 (updateReg18 regs2 (nth instlist (get regs2 "pc")) instmapb)]
      (if (= 1 (get newreg "hung" 0) (get newreg2 "hung" 0))
        [newreg newreg2]
        (recur  instlist newreg newreg2)))))

(defn day18b
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildInstList18 y [])]
    (println (runInstList18b  tree {"pc" 0 "p" 0 "mq" nil} {"pc" 0 "p" 1 "mq" nil}))))

