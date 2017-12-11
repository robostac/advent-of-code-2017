(ns day9)
(require '[clojure.string :as str])

(defn removeGarbage
  [input]
  (let [f (first input)
        n (next input)]
    (case f
      \! (recur (next n))
      \> n
      (recur n))))

(defn scoreGroups
  [input depth score]

  (let [f (first input)
        n (next input)]
  ;;(println f n  depth score)
    (if f
      (case f
        \! (recur (next n) depth score)
        \< (recur (removeGarbage n) depth score)
        \{ (recur n (inc depth) score)
        \} (recur n (dec depth) (+ score depth))
        (recur n depth score))
      score)))

(defn day9a
  []
  (println (scoreGroups (read-line) 0 0))) (defn countGarbage
                                             [input depth score]

                                             (let [f (first input)
                                                   n (next input)]
                                               (if f
                                                 (case f
                                                   \! (recur (next n) depth score)
                                                   \> (recur n (dec depth) score)
                                                   \< (if (= depth 0) (recur n (inc depth) score) (recur n depth (inc score)))
                                                   (recur n depth (if (> depth 0) (inc score) score)))
                                                 score)))

(defn day9b
  []
  (println (countGarbage (read-line) 0 0)))
