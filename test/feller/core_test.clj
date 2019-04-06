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
    (is (= (choose 4 2) 6.0))
    (is (= (choose 23 10) (float (/ (factorial 23) (factorial 10) (factorial 13)))))
    ))

(deftest occupancies-test
  (testing "occupancies fail."
    (is (= (occupancies 1 1) 1.0))
    (is (= (occupancies 2 1) 1.0))
    (is (= (occupancies 1 2) 2.0))
    (is (= (occupancies 1 3) 3.0))
    (is (= (occupancies 3 1) 1.0))
    (is (= (occupancies 3 2) 4.0))
    (is (= (occupancies 2 3) 6.0))
    (is (= (occupancies 5 3) 21.0))
    ))
