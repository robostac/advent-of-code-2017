(ns day23)
(require '[clojure.string :as str])

(defstruct inst23 :inst :x :y)

(defn instset23
  [regs a b tgt]
  (assoc regs
         tgt b))

(defn instsub23
  [regs a b tgt]
  (if (= tgt "h") (println regs))
  (assoc regs tgt  (- a b)))

(defn instmul23
  [regs a b tgt]
  (assoc regs
         tgt (* a b)
         "mulcnt" (inc (get regs "mulcnt" 0))))

(defn instjnz23
  [regs a b tgt]
  (if (= a 0)
    regs
    (assoc regs
           "pc" (+ (get regs "pc" 0) (- b 1)))))

(def instmapa23
  {"sub" instsub23
   "jnz" instjnz23
   "mul" instmul23
   "set" instset23})

(defn as-int23
  [s regs]
  (try
    (Integer. s)
    (catch NumberFormatException e
      (get regs s 0))))

(defn getInst23
  [in]
  (let [spl (str/split in #"\s")]
    (if (= (count spl) 2)
      (struct inst23 (first spl) (nth spl 1) 0)
      (struct inst23 (first spl) (nth spl 1) (nth spl 2)))))

(defn buildInstList23
  [in tree]
  (if in
    (let [p (getInst23 (first in))]
      (recur (next in) (conj tree p)))
    tree))

(defn updateReg23
  [initregs inst instmap]
  (let [regs (assoc initregs "pc" (inc (get initregs "pc" 0)))]
    (let [v1 (as-int23 (:x inst) regs)
          v2 (as-int23 (:y inst) regs)]
      ((get instmap (:inst inst)) regs v1 v2 (:x inst)))))

(defn runInstList23
  [instlist regs]
  (if  (>= (get regs "pc") (count instlist))
    regs
    (let [newreg (updateReg23 regs (nth instlist (get regs "pc")) instmapa23)]
      (recur instlist newreg))))

(defn day23a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildInstList23 y [])]
    (println (runInstList23 tree (hash-map "pc" 0)))))

(defn is-prime? [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

(defn day23b
  []
  ;count non primes between b and c. 
  ;hardcoded input
  (let [a  (+ 100000 (* 67 100))
        b (+ a 17000)]
    (println  (count (filter #(not (is-prime? %)) (range a (inc b) 17))))))
