(ns feller.core-test
  (:require [clojure.test :refer :all]
            [feller.core :refer :all]))

(deftest factorial-test
  (testing "factorial fail."
    (is (= (factorial 0) 1M))
    (is (= (factorial 1) 1M))
    (is (= (factorial 2) 2M))
    (is (= (factorial 3) 6M))
    ))

(deftest choose-test
  (testing "choose fail."
    (is (= (choose 4 2) 6))
    (is (= (choose 23 10) (int (/ (factorial 23) (factorial 10) (factorial 13)))))
    ))

(deftest occupancies-test
  (testing "occupancies fail."
    (is (= (occupancies 1 1) 1))
    (is (= (occupancies 2 1) 1))
    (is (= (occupancies 1 2) 2))
    (is (= (occupancies 1 3) 3))
    (is (= (occupancies 3 1) 1))
    (is (= (occupancies 3 2) 4))
    (is (= (occupancies 2 3) 6))
    (is (= (occupancies 5 3) 21))
    ))
