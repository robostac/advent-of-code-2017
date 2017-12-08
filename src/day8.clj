(ns day8)
(require '[clojure.string :as str])

(defstruct inst :reg :inst :val :cond_reg :cond_if :cond_value)

(defn getInst
  [in]
  (let [spl (str/split in #"\s")]
    (struct inst (first spl) (nth spl 1) (Integer. (nth spl 2)) (nth spl 4) (nth spl 5) (Integer. (nth spl 6)))))

(defn buildInstList
  [in tree]
  (if in
    (let [p (getInst (first in))]
      (recur (next in) (conj tree p)))
    tree))

(defn getCondIf
  [condif]
  (case condif
    "<=" <=
    ">=" >=
    "==" =
    "!=" not=
    "<" <
    ">" >))

(defn updateReg
  [regs inst]
  (let [value (get regs (:cond_reg inst) 0)
        cif (getCondIf (:cond_if inst))
        check (:cond_value inst)
        act (case (:inst inst)
              "dec" -
              "inc" +)
        reg (:reg inst)
        oldval (get regs reg 0)]
    (if (cif value check)
      (assoc regs reg (act oldval (:val inst)))
      regs)))

(defn runInstList
  [instlist regs mv]
  (if instlist
    (let [newreg (updateReg regs (first instlist))]
      (runInstList (next instlist) newreg (max mv (reduce max (vals regs)))))
    [regs mv]))

(defn day8a
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildInstList y [])]
        ;;(println (buildTree y (hash-map )))    
    (println (reduce max (vals (first (runInstList  tree (hash-map "a" 0) 0)))))))

(defn day8b
  []
  (let [y (into [] (line-seq (java.io.BufferedReader. *in*)))
        tree (buildInstList y [])]
  ;;(println (buildTree y (hash-map )))    
    (println (second (runInstList  tree (hash-map "a" 0) 0)))))
