(ns programming-bitcoin.ecc
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.math.numeric-tower :as math]
            [buddy.core.nonce :as nonce]
            [buddy.core.mac :as mac]
            [buddy.core.codecs :as codecs])
  (:import (java.math BigInteger)
           (java.util Random)))

;; Order of finite cyclic group of generator point G
(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)


(defn mod-expt
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))


(defprotocol FieldOps
  (+   [x y])
  (-   [x y])
  (*   [x y])
  (/   [x y])
  (pwr [x y])
  (zero  [x]))

(defn scalar-multiply [c p]
  ;; If coefficient is zero, return point at infinity
  (if (zero? c) (zero p)
      (loop [c c
             z (zero p)
             p p]
        (let [t (even? c)
              c (quot c 2)]
          (cond
            ;; if the coef is even, recur with the coef / 2, zero, and the point added to itself
            t (recur c z (+ p p))
            ;; if the coef is zero, return the point + the current zero value
            (zero? c) (+ p z)
            ;; if the coef is odd, recur with the coef /2, zero plus the point, and the point added to itself
            :else (recur c (+ p z) (+ p p)))))))

(declare S256Point?)

(extend-type Number
  FieldOps
  (+   [x y] (+' x y))
  (-   [x y] (-' x y))
  (*   [x y] (cond
               (number? y) (*' x y)
               (S256Point? y) (scalar-multiply (mod x N) y)
               :else (scalar-multiply x y)))
  (/   [x y] (clojure.core// x y))
  (pwr [x k] (math/expt x k))
  (zero [x] 0))


(defn rand-bigint
  "Returns a random integer with bitlength n."
  [n]
  (->> (new Random)
       (new BigInteger n)
       bigint))


(defn uniform-number
  "Return a random number that is between 1 and n-1"
  [n]
  (+ 1 (rand-bigint (->
                     (- n 2)
                     str
                     count))))

(defn prime?
  "Fermat based primality test"
  [n]
  (cond
    (= n 1) false
    (= n 2) true
    :else (let [pow (dec n)]
            (loop [k 50]
              (if (= k 0)
                true
                (let [res (mod-expt (uniform-number n) pow n)]
                  (if-not (= res 1)
                    false
                    (recur (dec k)))))))))

(defrecord FieldElement [num prime]
  FieldOps
  (+ [x {n2 :num p2 :prime}]
    (assert (= prime p2))
    (FieldElement. (mod (+' num n2) prime) prime))
  (- [x {n2 :num p2 :prime}]
    (assert (= prime p2))
    (FieldElement. (mod (-' num n2) prime) prime))
  (* [x {n2 :num p2 :prime}]
    (assert (= prime p2))
    (FieldElement. (mod (*' num n2) prime) prime))
  (/ [x {n2 :num p2 :prime}]
    (assert (= prime p2))
    (FieldElement. (mod (*' num (mod-expt n2 (-' prime 2) prime)) prime) prime))
  (pwr [x k]
    (let [k (mod k (dec prime))]
      (FieldElement. (mod-expt num k prime) prime)))
  (zero [x]
    (FieldElement. 0 prime)))


(defn slope
  "Calculates slope of a line"
  [x1 x2 y1 y2]
  (/ (- y2 y1) (- x2 x1)))


(defn tangent-slope
  "Calculates the slope of a tangent line to the elliptic curve"
  [x y a]
  (/ (+ (* 3 (pwr x 2)) a) (* 2 y)))


(defrecord Point [x y a b]
  FieldOps
  (+ [{x1 :x y1 :y a1 :a b1 :b, :as p1}
      {x2 :x y2 :y a2 :a b2 :b, :as p2}]
    (assert (and (= a1 a2) (= b1 b2)) "Points aren't on the same curve")
    (cond
      (nil? x1) p2
      (nil? x2) p1
      (and (= x1 x2) (not= y1 y2)) (->Point nil nil a1 b2)
      (not= x1 x2) (let [s (slope x1 x2 y1 y2)
                         x3 (- (- (pwr s 2) x1) x2)
                         y3 (-  (* s (- x1 x3)) y1)]
                     (Point. x3 y3 a1 b1))
      (= p1 p2) (let [s (tangent-slope x1 y1 a1)
                      x3 (- (- (pwr s 2) x1) x2)
                      y3 (- (* s (- x1 x3)) y1)]
                  (Point. x3 y3 a1 b1))))
  (zero [p] (Point. nil nil a b)))


(defn valid-point?
  [x y a b]
  (or (= x y nil)
      (= (pwr y 2) (+ (+ (pwr x 3) (* a x)) b))))


(defn ->Point
  [x y a b]
  (assert (valid-point? x y a b) "Point not on curve")
  (Point. x y a b))

(def P (- (- (pwr 2 256) (pwr 2 32)) 977))
(def A (->FieldElement 0 P))
(def B (->FieldElement 7 P))

(defn ->S256Point [x y]
  (let [x (if (number? x) (->FieldElement x P) x)
        y (if (number? y) (->FieldElement y P) y)]
    (->Point x y A B)))

(def G (->S256Point 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                    0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))

(defn S256Point? [{:keys [a b]}]
  (and (= a A) (= b B)))

(* N G)
;; => #programming_bitcoin.ecc.Point{:x nil, :y nil, :a #programming_bitcoin.ecc.FieldElement{:num 0, :prime 115792089237316195423570985008687907853269984665640564039457584007908834671663N}, :b #programming_bitcoin.ecc.FieldElement{:num 7, :prime 115792089237316195423570985008687907853269984665640564039457584007908834671663N}}


(defrecord Signature [r s])

(defn verify-sig [p z {:keys [r s]}]
  (let [s_inv (mod-expt s (- N 2) N)
        u (mod (* z s_inv) N)
        v (mod (* r s_inv) N)
        total (+ (* u G) (* v p))]
    (= r (:num (:x total)))))
;; => #'programming-bitcoin.ecc/verify-sig


(defrecord PrivateKey [secret point])

(defn ->PrivateKey [s]
  (PrivateKey. s (* s G)))

(defn num->bytes
  ([length n]
   (num->bytes [length n "big"]))
  ([length n end]
   (let [a  (.toByteArray (biginteger n))
         l  (count a)
         zs (repeat ('- length l) (byte 0))
         le (comp byte-array reverse)
         be byte-array
         >l (drop (-' l length) (seq a))
         <l (concat zs a)]
     (cond
       (and (> l length) (= end "big")) (be >l)
       (and (< l length) (= end "big")) (be <l)
       (and (> l length) (= end "little")) (le >l)
       (and (< l length) (= end "little")) (le <l)))))
;; => #'programming-bitcoin.ecc/num->bytes


(defn bytes->num
  ([bs]
   (bytes->num bs "big"))
  ([bs end]
   (let [bs (cond
              (= end "little") (reverse bs)
              (= end "big") bs)]
     (->> bs
          (into [0])
          byte-array
          BigInteger.))))
;; => #'programming-bitcoin.ecc/bytes->num

(defn rand-k []
  (let [x (bytes->num (nonce/random-bytes 32))]
    (if (< x N)
      x
      (recur))))
;; => #'programming-bitcoin.ecc/rand-k

(defn deterministic-k [secret z]
  (let [k (byte-array 32 (byte 0))
        v (byte-array 32 (byte 1))
        z (if (> z N) (- z N) z)
        z-bytes ()]))

(defn sign [{:keys [secret point]} z]
  (let [k (rand-k)
        r ((:num (:x (* k G))))
        k_inv (mod-expt k (- N 2) N)
        s (mod (* k_inv (+ z (* secret r))) N)]
    (if (> s (/ N 2))
      (let [s (- N s)]
        (->Signature r s)))))
;; => #'programming-bitcoin.ecc/sign
