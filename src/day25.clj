(ns day25)
(require '[clojure.string :as str])

(defrecord state [current value write dir next])

(defrecord tape [pos values current counter states])

(defn extractSubInst
  [lines cur]
  (->state
   cur
   (second (re-find #"If the current value is (.):" (nth lines 0)))
   (second (re-find #"- Write the value (.)" (nth lines 1)))
   (if (re-find #"- Move one slot to the right." (nth lines 2)) 1 -1)
   (second (re-find #" - Continue with state (.)." (nth lines 3)))))

(defn extractInst
  [lines]
  (let [cur (second (re-find #"In state (.):" (first lines)))]
    (if cur
      [(extractSubInst (nthnext lines 1) cur)
       (extractSubInst (nthnext lines 5) cur)]
      [])))

(defn setupTape
  [lines]
  (->tape
   0
   {}
   (some  #(second %) (map #(re-find #"Begin in state (.)." %) lines))
   (Integer. (some #(second %) (map #(re-find #"Perform a diagnostic checksum after (\d+) steps." %) lines)))
   (mapcat #(extractInst (nthnext lines %)) (range (count lines)))))

(defn nextState
  [tape]
  (let [state (some #(if (and (= (:current tape) (:current %)) (= (:value %) (get (:values tape) (:pos tape) "0"))) %)  (:states tape))]
    (assoc tape
           :counter (dec (:counter tape))
           :current (:next state)
           :values (assoc (:values tape) (:pos tape) (:write state))
           :pos (+ (:pos tape) (:dir state)))))

(defn forwardState
  [tape]
  (if (= (:counter tape) 0)
    (:values tape)
    (recur (nextState tape))))

(defn day25a
  []
  (let [y (line-seq (java.io.BufferedReader. *in*))]
    (println (count (filter #(= "1" %) (vals (forwardState (setupTape y))))))))

(defn day25b
  [])