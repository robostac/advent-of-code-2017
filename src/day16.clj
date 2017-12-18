(ns day16)
(require '[clojure.string :as str])

(defn switchDancers
  [dancers data]
  (let [p (- (count dancers) (Integer. data))
        newdancers (into [] (concat (subvec dancers p) (subvec dancers 0 p)))]
    newdancers))

(defn exchangeDancers
  [dancers data]
  (let [spl (str/split data #"/")
        n1 (Integer. (nth spl 0))
        n2 (Integer. (nth spl 1))
        v1 (nth dancers n1)
        v2 (nth dancers n2)
        newdancers1 (assoc dancers n1 v2)
        newdancers (assoc newdancers1 n2 v1)]
    newdancers))

(defn partnerDancers
  [dancers data]
  (let [n1 (.indexOf dancers (nth data 0))
        n2 (.indexOf dancers (nth data 2))
        v1 (nth dancers n1)
        v2 (nth dancers n2)
        newdancers1 (assoc dancers n1 v2)
        newdancers (assoc newdancers1 n2 v1)]
    newdancers))

(defn applyInst
  [inst dancers]
  (if (= (count inst) 0)
    dancers
    (let [itodo (first inst)
          i (first itodo)
          r (subs itodo 1)
          rem (next inst)]
      (case i
        \s (recur rem (switchDancers dancers r))
        \x (recur rem (exchangeDancers dancers r))
        \p (recur rem (partnerDancers dancers r))))))

(defn day16a
  []
  (let [dancers (into [] (read-line))
        inst (str/split (read-line) #",")]
    (println dancers)
    (println (str/join (applyInst inst dancers)))))

(def memo-apply (memoize applyInst))

(defn dobillion
  [inst dancers counter]
  (if (= counter 0)
    dancers
    (recur inst (memo-apply inst dancers) (dec counter))))

(defn day16b
  []
  (let [dancers (into [] (read-line))
        inst (str/split (read-line) #",")]
    (println (str/join (dobillion inst dancers (* 1000 1000000))))))