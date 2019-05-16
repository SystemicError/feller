(ns feller.german-tanks
  (:require [feller.core :refer :all]))

(defn bayesian-update [prior-dist observations domain]
  (if (empty? observations)
    prior-dist ; no observations means prior distribution unchanged
    (let [k (first observations)
          pr-e-given-prior (fn [n] (if (> k n)
                                     0
                                     (/ 1.0 n)))
          pr-e (reduce + (for [r (range domain)]
                           (* (prior-dist r) (pr-e-given-prior r))))
          posterior-dist (fn [n] (/ (* (pr-e-given-prior n) (prior-dist n)) pr-e))]
      (recur posterior-dist (rest observations) domain))))

(defn german-tanks [observations]
  (let [domain 75
        prior-dist (fn [n] (if (and (< 0 n) (>= domain n)) (/ 1.0 domain) 0.0))]
    (bayesian-update prior-dist observations domain)))


(defn graph-dist [dist cols domain]
  (let [maximum (reduce max (map dist (range domain)))
        num-to-bar (fn [x s] (if (> x 0)
                               (recur (dec x) (str s "#"))
                               (str s "\n")))
        dummy (println (str "max " maximum "\n"))]
    (println (map #(num-to-bar (/ (* (dist %) cols) maximum) "") (range domain)))))

(defn expected-value [dist domain]
  (reduce + (map #(* % (dist %)) (range domain))))

;(defn variance [dist domain]
