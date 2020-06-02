(ns programming-bitcoin.ecc-test
  (:refer-clojure :exclude [+ - * /])
  (:require
   [clojure.test :refer :all]
   [programming-bitcoin.ecc :refer :all]))


(deftest exponentiation
  (testing "mod-expt"
    (testing "small positive integers"
      (is (= (mod-expt 2 5 7)  (.modPow (biginteger 2) (biginteger 5) (biginteger 7)))))
    (testing "large exponent and base"
      (is (= (mod-expt 2000 1000 7) (.modPow (biginteger 2000) (biginteger 1000) (biginteger 7)))))))

(deftest primality
  (testing "prime?"
    (testing "1"
      (is (= (prime? 1) false)))
    (testing "2"
      (is (= (prime? 2) true)))
    (testing "4"
      (is (= (prime? 4) false)))
    (testing "1729 - Carmichael Number"
      (is (= (prime? 1729) false)))
    (testing "massive prime"
      (is (= (prime? 290245329165570025116016487217740287508837913295571609463914348778319654489118435855243301969001872061575755804802874062021927719647357060447135321577028929269578574760547268310055056867386875959045119093967972205124270441648450825188877095173754196346551952542599226295413057787340278528252358809329N) true)))))

(deftest field-elements
  (testing "make-fe"
    (is (= (make-fe 0 7)  (->FieldElement 0 7)))
    (is (= (make-fe 1 7)  (->FieldElement 1 7)))
    (is (= (make-fe 6 31) (->FieldElement 6 31))))
  (testing "field-ops"
    (let [p 31
          a (->FieldElement 2 p)
          b (->FieldElement 2 p)
          c (->FieldElement 15 p)
          d (->FieldElement 17 p)
          e (->FieldElement 21 p)
          f (->FieldElement 30 p)
          g (->FieldElement 24 p)
          h (->FieldElement 19 p)
          i (->FieldElement 3 p)
          j (->FieldElement 4 p)
          k (->FieldElement 11 p)
          l (->FieldElement 5 p)
          m (->FieldElement 18 p)]
      (testing "="
        (is (= (= a b) true))
        (is (= (= a c) false)))
      (testing "+"
        (is (= (+ a c) (->FieldElement 17 31)))
        (is (= (+ d e) (->FieldElement 7 31))))
      (testing "-"
        (is (= (- c b) (->FieldElement 13 31)))
        (is (= (- c f) (->FieldElement 16 31))))
      (testing "*"
        (is (= (* g h) (->FieldElement 22 31))))
      (testing "expt"
        (is (= (expt d 3)         (->FieldElement 15 31)))
        (is (= (* m (expt l 5)))) (->FieldElement 16 31))
      (testing "/"
        (is (= (/ i g)            (->FieldElement 4 31)))
        (is (= (expt d -3)        (->FieldElement 29 31)))))))

(deftest curve-points
  (testing "make-pt"
    (is (= (make-pt -1 -1 5 7)       (->Point -1 -1 5 7)))
    (is (= (make-pt 18 77 5 7)       (->Point 18 77 5 7)))
    (is (= (make-pt ##Inf ##Inf 5 7) (->Point ##Inf ##Inf 5 7))))
  (testing "point-ops"
    (let [a (->Point ##Inf ##Inf 5 7)
          b (->Point 2 5 5 7)
          c (->Point 2 -5 5 7)
          d (->Point 3 7 5 7)
          e (->Point -1 -1 5 7)
          f (->Point 18 77 5 7)]
      (testing "point at infinity + point"
        (is (= (+ a b) b))
        (is (= (+ b c) a))
        (testing "commutativity"
          (is (= (+ b a) b))))
      (testing "different points"
        (is (= (+ d e) c)))
      (testing "same point"
        (is (= (+ e e) f))))))
