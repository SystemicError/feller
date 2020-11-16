(ns feller.zonk
  (:require [feller.core :refer :all]
            [clojure.math.combinatorics :as combo]))

(defn roll-to-histogram
  "Counts for each face."
  ([roll] (roll-to-histogram roll (list 0 0 0 0 0 0)))
  ([roll histogram]
   (if (empty? roll)
     histogram
     (let [f (first roll)
           index (dec f)
           head (take index histogram)
           tail (drop f histogram)
           ]
       (recur (rest roll) (concat head [(inc (nth histogram index))] tail))))))

(defn triples?
  "Does a given roll have triples (possibly twice)?  If so, return the score on them."
  [roll]
  (if (> 3 (count roll))
    0
    (let [sorted (sort roll)]
      (if (= (first sorted) (nth sorted 1) (nth sorted 2))
        (let [n (count (filter #(= (first sorted) %) sorted))
              exponent (- n 3)
              face (first sorted)
              principal (if (= 1 face)
                          1000
                          (* (first sorted) 100))
              ]
          (+ (* principal (Math/pow 2.0 exponent))
             (triples? (drop n sorted))))
        (recur (drop 1 sorted))))))

(defn three-pairs?
  "Does a given roll have three pairs?"
  [roll]
  (let [sorted (sort roll)]
    (and (= 6 (count roll))
         (= (first sorted) (nth sorted 1))
         (= (nth sorted 2) (nth sorted 3))
         (= (nth sorted 4) (nth sorted 5)))))

(defn prob-zonk
  "The probability of zonking on n dice."
  [n]
  (let [no-ones-or-fives (apply combo/cartesian-product (for [i (range n)] (list 2 3 4 6)))
        total (Math/pow 6.0 n)
        no-triples (filter #(= 0 (triples? %)) no-ones-or-fives)
        no-three-pairs (filter #(not (three-pairs? %)) no-triples)
        zonks (count no-three-pairs)
        ]
    (/ zonks total)))

(defn max-score
  "The maximum score extracted from a single set of dice (no rerolling)."
  [roll]
  (let [histogram (roll-to-histogram roll)
        sorted (sort roll)
        ; This case can't coincide with others
        straight (if (= sorted (range 1 7)) 1500 0)
        ; This case can't coincide with others
        three-pairs (if (three-pairs? sorted) 1000 0)

        ; below this line is all a third case, as multiple things can happen at once
        triple-score (triples? sorted)
        ; if the triples aren't the entire roll, score the remainder
        histogram-less-triples (map #(if (< 2 %) 0 %) histogram)
        triple-with-bonus (+ triple-score
                             (* 100 (first histogram-less-triples))
                             (* 50 (nth histogram-less-triples 4)))
        ]
    (max straight three-pairs triple-with-bonus)
    ))

(defn expected-score
  "The expected score (without rerolls) from rolling n dice."
  [n]
  (let [rolls (apply combo/cartesian-product (for [i (range n)] (list 1 2 3 4 5 6)))
        scores (map max-score rolls)
        ]
    (/ (reduce + scores) (count rolls))))

(defn hold-points
  "Show the score you need to have with n dice in order to justify holding.  The score includes the current max-score for the showing dice."
  []
  (map #(/ (expected-score %) (prob-zonk %)) (range 1 7)))
