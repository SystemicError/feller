(ns feller.core)

(defn factorial
  ([n] (factorial n 1M))
  ([n product]
   (if (= 0 n)
     product
     (if (< 0 n)
       (recur (dec n) (* n product))))))

(defn choose
  ([n r]
   (if (< r 0)
     0
     (choose n r 1)))
  ([n r product]
   (if (= 0 r)
     (int product)
     (recur (dec n) (dec r) (/ (* product n) r)))))

(defn occupancies [n r]
  "Number of ways to put n identical balls in r urns."
  (int (choose (+ n r -1) (dec r))))
