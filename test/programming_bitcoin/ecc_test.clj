(ns programming-bitcoin.ecc-test
  (:require
   [clojure.test :refer :all]
   [programming-bitcoin.ecc :refer :all]))


(deftest exponentiation
  (testing "mod**"
    (testing "small positive integers"
      (is (= (mod** 2 5 7)  (.modPow (biginteger 2) (biginteger 5) (biginteger 7)))))
    (testing "large exponent and base"
      (is (= (mod** 2000 1000 7) (.modPow (biginteger 2000) (biginteger 1000) (biginteger 7))))))
  (testing "**"
    (testing "8**3"
      (is (= (** 8 3) (* 8 8 8))))
    (testing "1**99"
      (is (= (** 1 99) 1)))
    (testing "0**99"
      (is (= (** 0 99) 0)))
    (testing "99**1"
      (is (= (** 99 1) 99)))))

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

(deftest field-ops
  (let [a (->FieldElement 2 31)
        b (->FieldElement 2 31)
        c (->FieldElement 15 31)
        d (->FieldElement 17 31)
        e (->FieldElement 21 31)
        f (->FieldElement 30 31)]
    (testing "=f"
      (is (= (=f a b) true))
      (is (= (=f a c) false)))
    (testing "+f"
      (is (= (+f a c) (->FieldElement 17 31)))
      (is (= (+f d e) (->FieldElement 7 31))))
    (testing "-f"
      (is (= (-f c b) (->FieldElement 13 31)))
      (is (= (-f c f) (->FieldElement 16 31))))))
