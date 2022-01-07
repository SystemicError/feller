(ns feller.best-streak
  (:require [feller.core :refer :all]
            [clojure.math.combinatorics :as combo]))

(defn find-best-streak
  "Find the longest streak on a list of trials."
  ([trials] (find-best-streak trials 0 0))
  ([trials best current]
   (if (empty? trials)
     best
     (if (first trials)
       (if (<= best current)
         (recur (rest trials) (inc current) (inc current))
         (recur (rest trials) best (inc current)))
       (recur (rest trials) best 0)))))

(defn generate-trials [num-trials num-wins]
  "Generate all orderings of num-wins in num-trials."
  (combo/permutations (for [i (range num-trials)] (if (< i num-wins) true false))))

(defn best-streak-distribution-for-num-wins [num-trials num-wins]
  "Calculate how many streaks of num-wins wins in num-trials trials have each of 0,1,2,...num-wins as their best streak."
  (let [trials (generate-trials num-trials num-wins)
        bests (map find-best-streak trials)
        ]
    (map (fn [i] (count (filter #(= i %) bests))) (range 1 (inc num-wins)))))

(defn best-streak-distribution-for-rate
  "Calculate the probability distribution of streaks for a given win rate and number of trials."
  ([num-trials win-rate] (best-streak-distribution-for-rate
                           num-trials
                           win-rate
                           (for [i (range num-trials)]
                             (for [j (range num-trials)]
                               (if (= i j 0) 1 0)))))
     ; The distribution is a 2d array of probabilities indexed by best then current win streaks.
  ([num-trials win-rate distribution]
   (if (= num-trials 0)
     ; in the base case, sum each row of bests and return
     (map #(reduce + %) distribution)
     ; otherwise, build the next iteration of distribution
     (let [loss-rate (- 1 win-rate)
           prob (fn [best current]
                  (if (> current best)
                    0
                    (if (= 0 current)
                      (* loss-rate (reduce + (nth distribution best)))
                      (* win-rate (+ (nth (nth distribution best) (dec current))
                                     (if (= current best)
                                       (nth (nth distribution (dec best)) (dec current))
                                       0))))))
           new-distribution (for [best (range (count distribution))]
                              (for [current (range (count (first distribution)))]
                                (prob best current)))
           ]
       (recur (dec num-trials) win-rate new-distribution)))))
