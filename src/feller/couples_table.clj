(ns feller.couples-table
  (:require [feller.core :refer :all]))

(defn count-couples
  "Counts m/w transitions across linear table."
  ([table] (count-couples table 0 (first table)))
  ([table total prev]
   (if (empty? table)
     total
     (if (= (first table) prev)
       (recur (rest table) total (first table))
       (recur (rest table) (inc total) (first table))))))

(defn generate-table
  "Generates a random table of m 1s and w 0s."
  ([m w] (generate-table m w []))
  ([m w table]
   (if (= 0 m w)
     table
     (let [r (* (rand) (+ m w))]
       (if (< r m)
         (recur (dec m) w (cons 1 table))
         (recur m (dec w) (cons 0 table)))))))

(defn simulate-avg-couples
  "Simulate n tables and average the number of couples."
  ([m w n] (simulate-avg-couples m w n 0 0))
  ([m w n couples trials]
   (if (= trials n)
     (/ couples n)
     (let [table (generate-table m w)
           couple-count (count-couples table)]
       (recur m w n (+ couples couple-count) (inc trials))))))


(defn num-tables-with-couple-count [m w c]
  "How many tables with c couples?"
  (let [max-m-runs (min (inc w) m)
        max-w-runs (min (inc m) w)
        max-couples (* 2 (min max-m-runs max-w-runs))]
    (if (> c max-couples)
      0
      (if (odd? c)
        ;only options are mixed endpoints
        (let [runs (/ (inc c) 2)]
          (* 2 ; because we could start with m or w
             (occupancies (- m runs) runs)
             (occupancies (- w runs) runs)))
        ; if c even
        (let [end-like-runs (/ (+ 2 c) 2)
              mid-like-runs (dec end-like-runs)
              m-ends (* (occupancies (- m end-like-runs) end-like-runs)
                        (occupancies (- w mid-like-runs) mid-like-runs))
              w-ends (* (occupancies (- w end-like-runs) end-like-runs)
                        (occupancies (- m mid-like-runs) mid-like-runs))]
          (+ m-ends w-ends))))))

(defn calculate-avg-couples [m w]
  "Calculate the avg number of couples."
  (let [couple-counts (range (* 2 (+ m w)))
        table-counts (for [c couple-counts] (num-tables-with-couple-count m w c))
        totals (map * couple-counts table-counts)]
    (/ (apply + totals)
       (choose (+ m w) m))))
