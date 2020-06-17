(ns programming-bitcoin.ecc-test
  (:refer-clojure :exclude [+ - * /])
  (:require
   [clojure.test :refer :all]
   [programming-bitcoin.ecc :refer :all]))


(deftest exponentiation
  (testing "pwr"
    (testing "integers"
      (is (= (pwr 3 2) 9))
      (is (= (pwr 0 1) 0))
      (is (= (pwr 2 0) 1))
      (is (= (pwr 2 1) 2))))
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
      (testing "pwr"
        (is (= (pwr d 3)         (->FieldElement 15 31)))
        (is (= (* m (pwr l 5)))) (->FieldElement 16 31))
      (testing "/"
        (is (= (/ i g)    (->FieldElement 4 31)))
        (is (= (pwr d -3) (->FieldElement 29 31)))))))

(deftest curve-points
  (testing "->Point"
    (is (= (->Point  -1 -1  5 7) (map->Point {:x -1  :y -1  :a 5 :b 7})))
    (is (= (->Point  18 77  5 7) (map->Point {:x 18  :y 77  :a 5 :b 7})))
    (is (= (->Point nil nil 5 7) (map->Point {:x nil :y nil :a 5 :b 7}))))
  (testing "point addition over reals"
    (let [a (->Point nil nil 5 7)
          b (->Point   2   5 5 7)
          c (->Point   2  -5 5 7)
          d (->Point   3   7 5 7)
          e (->Point  -1  -1 5 7)
          f (->Point  18  77 5 7)]
      (testing "point at infinity + point"
        (is (= (+ a b) b))
        (is (= (+ b c) a))
        (testing "commutativity"
          (is (= (+ b a) b))))
      (testing "different points"
        (is (= (+ d e) c)))
      (testing "same point"
        (is (= (+ e e) f)))))
  (let [prime 223
        a  (->FieldElement   0 prime)
        b  (->FieldElement   7 prime)
        x1 (->FieldElement 192 prime)
        y1 (->FieldElement 105 prime)
        x2 (->FieldElement  17 prime)
        y2 (->FieldElement  56 prime)
        x3 (->FieldElement 170 prime)
        y3 (->FieldElement 142 prime)
        x4 (->FieldElement  47 prime)
        y4 (->FieldElement  71 prime)
        x5 (->FieldElement  36 prime)
        y5 (->FieldElement 111 prime)
        x6 (->FieldElement  15 prime)
        y6 (->FieldElement  86 prime)
        inf (->Point nil nil  a b)
        p1  (->Point  x1  y1  a b)
        p2  (->Point  x2  y2  a b)
        p3  (->Point  x3  y3  a b)
        p4  (->Point  x4  y4  a b)
        p5  (->Point  x5  y5  a b)
        p6  (->Point  x6  y6  a b)]
    (testing "point addition over finite fields"
      (testing "different points"
        (is (= (+ p1 p2) p3)))
      (testing "same points"
        (is (= (+ p4 p4) p5))))
    (testing "scalar multiplication"
      (testing "multiplying by 0"
        (is (= (* 0 p1) inf)))
      (testing "multiplying by 1"
        (is (= (* 1 p4) p4)))
      (testing "multiplying until point at infinity"
        (is (= (* 7 p6) inf))))))

(deftest points-on-curve
  (let [prime 223
        a (->FieldElement 0 prime)
        b (->FieldElement 7 prime)
        valid-a [192 105]
        valid-b [17 56]
        valid-c [1 193]
        invalid-a [200 119]
        invalid-b [42 99]]))

(deftest S256-points
  (testing "initializing S256 point"
    (testing "at point at infinity"
      (is (= (->S256Point nil nil) (->Point nil nil A B))))
    (testing "normal point"
      (is (= (->S256Point 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                          0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)
             (->Point (->FieldElement 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798 P)
                      (->FieldElement 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8 P)
                      A
                      B))))))

(deftest random-number-generation
  (testing "range"
    (is (every? #(and (> % 0) (< % (pwr 2 256))) (repeat 1000 (rand-256))))))


(deftest signature-verification
  (testing "verifying private key signature"
    (let [pk (->PrivateKey (rand-256))
          z (rand-k)
          sig (sign pk z)
          p (:point pk)]
      (is (true? (verify-sig p z sig))))))
