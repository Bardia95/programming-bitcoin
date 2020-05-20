(ns programming-bitcoin.ecc-test
  (:require
   [clojure.test :refer :all]
   [programming-bitcoin.ecc :refer :all]))

(deftest exponentiation
  (testing "modpow"
    (testing "small positive integers"
      (is (= (modpow 2 5 7)  (.modPow (biginteger 2) (biginteger 5) (biginteger 7)))))
    (testing "large exponent and base"
      (is (= (modpow 2000 1000 7) (.modPow (biginteger 2000) (biginteger 1000) (biginteger 7))))))
  (testing "**"
    (testing "8**3"
      (is (= (** 8 3) (* 8 8 8))))
    (testing "1**99"
      (is (= (** 1 99) 1)))
    (testing "0**99"
      (is (= (** 0 99) 0)))
    (testing "99**1"
      (is (= (** 99 1) 99)))))
