(ns feller.couples-table-test
  (:require [clojure.test :refer :all]
            [feller.couples-table :refer :all]))

(deftest count-couples-test
  (testing "count-couples fail."
    (is (= (count-couples [0 1 0]) 2))
    (is (= (count-couples [0 1 1]) 1))
    (is (= (count-couples [1 0 1 1]) 2))
    ))

(deftest generate-table-test
  (testing "generate-table fail."
    (let [table (generate-table 9 12)]
      (is (= (count table) 21))
      (is (= (count (filter #(= 0 %) table)) 12))
      (is (= (count (filter #(= 1 %) table)) 9))
      )
    ))

(deftest simulate-avg-couples-test
  (testing "simulate-avg-couples fail."
    (let [avg (simulate-avg-couples 5 8 100)]
      (is (<= 0 avg))
      )
    ))

(deftest num-tables-with-couple-count-test
  (testing "num-tables-with-couple-count fail."
    (is (= (num-tables-with-couple-count 1 1 0) 0))
    (is (= (num-tables-with-couple-count 1 1 1) 2))
    (is (= (num-tables-with-couple-count 1 1 2) 0))
    (is (= (num-tables-with-couple-count 1 1 3) 0))
    (is (= (num-tables-with-couple-count 2 1 0) 0))
    (is (= (num-tables-with-couple-count 2 1 1) 2))
    (is (= (num-tables-with-couple-count 2 1 2) 1))
    (is (= (num-tables-with-couple-count 2 2 0) 0))
    (is (= (num-tables-with-couple-count 2 2 1) 2))
    (is (= (num-tables-with-couple-count 2 2 2) 2))
    (is (= (num-tables-with-couple-count 2 2 3) 2))
    (is (= (num-tables-with-couple-count 2 2 4) 0))
    (is (= (num-tables-with-couple-count 3 2 4) 1))
    ))

(deftest calculate-avg-couples-test
  (testing "calculate-avg-couples-test fail."
    (is (= (calculate-avg-couples 5 0) 0))
    (is (= (calculate-avg-couples 0 5) 0))
    (is (= (calculate-avg-couples 1 1) 1))
    (is (= (calculate-avg-couples 2 1) (/ 4 3)))
    (is (= (calculate-avg-couples 1 2) (/ 4 3)))
    (is (= (calculate-avg-couples 2 2) 2))
    (is (= (calculate-avg-couples 3 1) (/ 3 2)))
    (is (= (calculate-avg-couples 3 2) (/ 12 5)))
    ))
