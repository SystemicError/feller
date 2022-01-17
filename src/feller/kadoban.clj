(ns feller.kadoban
  (:require [feller.core :refer :all]
            [clojure.math.combinatorics :as combo]))

(def phi (/ (+ 1.0 (Math/sqrt 5.0)) 2.0))
(def psi (- 1 phi))

(defn geometric-sum [r] (/ 1.0 (- 1.0 r)))

(defn fib [n] (/ (- (Math/pow phi n) (Math/pow psi n)) (- phi psi)))

(defn approximate-expected-value-kadoban
  "Calculate approximate expected value of first streak of three."
  ([n] (approximate-expected-value-kadoban n 3 0))
  ([n k sum]
   (if (< n k)
     sum
     (recur n
            (inc k)
            (+ sum
               (/ (* k (fib (- k 2))) (Math/pow 2.0 (dec k))))))))

(defn first-streak [streak trials]
  "Returns the index of the first streak-length set of matches in trials."
  (if (apply = (take streak trials))
    (dec streak)
    (if (= 3 (count trials))
      ##NaN
      (inc (first-streak streak (rest trials))))))

(defn simulate-runs [streak n]
  "Returns probability distribution of trials ending with a streak-length tail of matches."
  (let [runs (combo/selections (list 0 1) n)
        lengths (map #(first-streak streak %) runs)
        num-trials (Math/pow 2 n)
        run-count (fn [run-length] (count (filter #(= run-length %) lengths)))
        number-of-runs-ending-at-index (map run-count (range num-trials))
        dummy (println (str "runs: " (into [] runs) "\n"
                            "lengths: " (into [] lengths) "\n"
                            "num-trials: " num-trials "\n"
                            "number-of-runs-ending-at-index: " (into [] number-of-runs-ending-at-index) "\n"))
        ]
    (map #(/ % num-trials) number-of-runs-ending-at-index)))

(defn expected [distribution]
  "finds the expected value by dot production of (1,2,3,....n) with probability distribution."
  (reduce + (map * dist (range 1 (inc (count dist))))))
