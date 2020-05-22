(ns programming-bitcoin.ecc
  (:import (java.math BigInteger)
           (java.util Random)))


(defn **
  "Big integer exponentiation"
  [b e]
  (reduce * (repeat (bigint e) (bigint b))))


(defn mod**
  "Runs modulo on every round of self-multiplication"
  [b e m]
  (cond (= e 0) 1
        (even? e) (rem (** (mod** b (/ e 2) m) 2) m)
        :else (rem (* b (mod** b (dec e) m)) m)))


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
                (let [res (mod** (uniform-number n) pow n)]
                  (if-not (= res 1)
                    false
                    (recur (dec k)))))))))


(defrecord FieldElement [e p])


(defprotocol FieldOps
  (=f    [x y])
  (+f    [x y])
  (-f    [x y])
  (*f    [x y])
  (divf  [x y])
  (**f   [x k]))


(defn make-fe
  "Constructor function for Field Element with validations"
  [e p]
  (assert (and (<= 0 e) (< e p) (prime? p)) "Invalid Field Element")
  (FieldElement. e p))


(defn assert=
  "Field equality assertion"
  [p p2]
  (assert (= p p2) "Fields need to be of the same prime order"))


(extend-type FieldElement
  FieldOps
  (=f [{e :e p :p} {e2 :e p2 :p}]
    (and (= e e2) (= p p2)))
  (+f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (make-fe (mod (+ e e2) p) p))
  (-f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (make-fe (mod (- e e2) p) p))
  (*f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (make-fe (mod (* e e2) p) p))
  (divf [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (make-fe (int (mod (* e (mod** e2 (- p 2) p)) p)) p))
  (**f [{e :e p :p} k]
    (let [k (mod k (dec p))]
      (make-fe (mod** e k p) p))))


(defrecord Point [x y a b])


(defprotocol PointOps
  (+p [x y]))


(defn on-curve?
  "Checks if point is on elliptic curve"
  [x y a b]
  (= (int (** y 2)) (int (+ (** x 3) (* a x) b))))


(defn make-pt
  "Constructor function for elliptic curve points with validations"
  [x y a b]
  (if (and (= x ##Inf) (= y ##Inf))
    (Point. x y a b)
    (do
      (assert (on-curve? x y a b))
      (Point. x y a b))))


(defn slope
  "Calculates slope of a line"
  [x1 x2 y1 y2]
  (int (/ (- y2 y1) (- x2 x1))))


(defn tangent-slope
  "Calculates the slope of a tangent line to the elliptic curve"
  [x y a]
  (int (/ (+ (* 3 (** x 2)) a) (* 2 y))))


(extend-type Point
  PointOps
  (+p [{x1 :x y1 :y a1 :a b1 :b, :as p1}
       {x2 :x y2 :y a2 :a b2 :b, :as p2}]
    (assert (and (= a1 a2) (= b1 b2)) "Points aren't on the same curve")
    (cond
      (= x1 ##Inf) p2
      (= x2 ##Inf) p1
      (and (= x1 x2) (not= y1 y2)) (make-pt ##Inf ##Inf a1 b2)
      (not= x1 x2) (let [s (slope x1 x2 y1 y2)
                         x3 (- (int (** s 2)) x1 x2)
                         y3 (-  (* s (- x1 x3)) y1)]
                     (make-pt x3 y3 a1 b1))
      (and (= x1 x2) (= y1 y2)) (let [s (tangent-slope x1 y1 a1)
                                      x3 (- (int (** s 2)) x1 x2)
                                      y3 (- (* s (- x1 x3)) y1)]
                                  (make-pt x3 y3 a1 b1)))))
