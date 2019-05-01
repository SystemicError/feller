(ns feller.repeat-samples
  (:require [feller.core :refer :all]))

(defn calculate-no-repeats [sample-size population]
  "Returns chances that this many samples with replacement will produce repeats."
  (/ (* (factorial sample-size) (choose population sample-size)) (Math/pow population sample-size)))

(defn simulate-no-repeats
  "Simulate n tables and average the number of couples."
  ([sample-size population n] (simulate-no-repeats sample-size population n 0 0))
  ([sample-size population n successes trials]
   (if (= trials n)
     (/ successes n)
     (let [samples (for [i (range sample-size)] (int (* (rand) population)))
           new-successes (if (= (count (clojure.core/set samples)) sample-size) (inc successes) successes)]
       (recur sample-size population n new-successes (inc trials))))))

