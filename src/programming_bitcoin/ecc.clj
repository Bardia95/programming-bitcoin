(ns programming-bitcoin.ecc
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.math.numeric-tower :as math]
            [buddy.core.nonce :as nonce]
            [buddy.core.mac :as mac]
            [buddy.core.codecs :as codecs]
            [buddy.core.hash :refer [sha256]])
  (:import (java.math BigInteger)
           (java.util Random)))


(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)


(defn mod-expt
  {:doc "Given a base, exponent, and modulo,
          returns b ^ e mod m"}
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))


(defprotocol FieldOps
  (+   [x y])
  (-   [x y])
  (*   [x y])
  (/   [x y])
  (pwr [x y])
  (zero  [x]))


(defn scalar-multiply [s p]
  (if (zero? s) (zero p)
      (loop [s s
             p1 p
             p2 (zero p1)]
        (let [e (even? s)
              s (quot s 2)]
          (if (zero? s)
            (+ p1 p2)
            (if e
              (recur s (+ p1 p1) p2)
              (recur s (+ p1 p1) (+ p1 p2))))))))


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
  {:doc "Returns a random integer with bitlength n."}
  [n]
  (->> (Random.)
       (BigInteger. n)
       biginteger))


(defn uniform-number
  {:doc "Return a random number that is between 1 and n-1"}
  [n]
  (+ 1 (rand-bigint (-> (- n 2)
                        str
                        count))))


(defn prime?
  {:doc "Fermat based primality test"}
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
  {:doc "Calculates slope of a line"}
  [x1 x2 y1 y2]
  (/ (- y2 y1) (- x2 x1)))


(defn tangent-slope
  {:doc "Calculates the slope of a tangent line to the elliptic curve"}
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


(defrecord Signature [r s])


(defrecord PrivateKey [secret point])


(defn ->PrivateKey [s]
  (PrivateKey. s (* s G)))


(defn num->bytes [length n]
  (let [a (.toByteArray (biginteger n))
        l (count a)
        zeros (repeat (- length l) (byte 0))]
    (if (> l length)
      (byte-array (drop (- l length) (seq a)))
      (byte-array (concat zeros a)))))


(defn bytes->num
  [bs]
  (->> bs
       (into [0])
       byte-array
       BigInteger.))


(defn rand-256 []
  (bytes->num (nonce/random-bytes 32)))


(defn rand-k []
  (let [x (bytes->num (nonce/random-bytes 32))]
    (if (< x N)
      x
      (recur))))


(defn hmac [key message]
  (-> (mac/hash message {:key key :alg :hmac+sha256})))


(defn deterministic-k [secret z]
  (let [k (byte-array 32 (byte 0))
        v (byte-array 32 (byte 1))
        z (if (> z N) (- z N) z)
        z-bytes (num->bytes 32 z)
        secret-bytes (num->bytes 32 secret)
        k (hmac k (byte-array (concat v [0] secret-bytes z-bytes)))
        v (hmac k v)
        k (hmac k (byte-array (concat v [1] secret-bytes z-bytes)))
        v (hmac k v)]
    (loop [k k
           v v]
      (let [v (hmac k v)
            candidate (bytes->num v)]
        (if (and (<= 1 candidate) (< candidate N))
          candidate
          (recur (hmac k (byte-array (concat v [0])))
                 (hmac k v)))))))


(defn sign [{:keys [secret point]} z]
  (let [k (deterministic-k secret z)
        r (:num (:x (* k G)))
        k_inv (mod-expt k (- N 2) N)
        s (mod (* k_inv (+ z (* secret r))) N)]
    (if (> s (/ N 2))
      (let [s (- N s)]
        (->Signature r s))
      (->Signature r s))))


(defn verify-sig [p z {:keys [r s]}]
  (let [s_inv (mod-expt s (- N 2) N)
        u (mod (* z s_inv) N)
        v (mod (* r s_inv) N)
        total (+ (* u G) (* v p))]
    (= r (:num (:x total)))))
