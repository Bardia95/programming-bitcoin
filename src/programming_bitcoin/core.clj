(ns programming-bitcoin.core)

(defn modpow [b e m]
  (mod (reduce #(mod (* %1 %2) m) (repeat e b)) m))

(defn ** [b e]
  (reduce * (repeat e b)))

(defn fermat-test [n]
  (let [a (inc (rand-int (dec n)))]
    (= (modpow a n n) a)))

(defn prime? [n]
  (every? true? (take 50 (repeatedly #(fermat-test n)))))

(defn make-finite-field [p]
  (if (prime? p)
    (into (sorted-set) (range 0 p))
    "Please supply a prime"))

(defprotocol FieldOperations
  (=f    [x y])
  (+f    [x y])
  (-f    [x y])
  (*f    [x y])
  (divf  [x y])
  (**f   [x k]))

(defn assert= [p p2]
  (assert (= p p2) "Fields need to be of the same prime order"))


(defrecord FieldElement [e p])

(extend-type FieldElement
  FieldOperations
  (=f [{e :e p :p} {e2 :e p2 :p}]
    (and (= e e2) (= p p2)))
  (+f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (mod (+ e e2) p) p))
  (-f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (mod (- e e2) p) p))
  (*f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (mod (* e e2) p) p))
  (divf [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (int (mod (* e (modpow e2 (- p 2) p)) p)) p))
  (**f [{e :e p :p} k]
    (let [k (mod k (dec p))]
      (FieldElement. (modpow e k p) p))))


(defn make-field-element [e p]
  (if (and (<= 0 e) (< e p) (prime? p))
    (FieldElement. e p)
    (println "Invalid Field Element")))


(defrecord Point [x y a b])

(defprotocol PointOperations
  (+p    [x y]))

(defn make-point [x y a b]
  (if (and (= x nil) (= y nil))
    (Point. x y a b)
    (if (not= (** y 2) (+ (** x 3) (* a x) b))
      (println (str "(" x ", " y ") is not on the curve."))
      (Point. x y a b))))

(extend-type Point
  PointOperations
  (+p [{x1 :x y1 :y a1 :a b1 :b}
       {x2 :x y2 :y a2 :a b2 :b}]
    (if (or (not= a1 a2) (not= b1 b2))
      (println "The points are not on the same curve")
      (if (= x1 nil)
        (Point. x2 y2 a2 b2)
        (if (= x2 nil)
          (Point. x1 y1 a1 b1)
          (if (and (= x1 x2) (not= y1 y2))
            (Point. nil nil a1 b2)
            (if (not= x1 x2)
              (let [s (/ (- y2 y1) (- x2 x1))
                    x3 (- (** s 2) x1 x2)
                    y3 (-  (* s (- x1 x3)) y1)]
                (Point. x3 y3 a1 b1))
              (if (and (= x1 x2) (= y1 y2))
                (let [s (/ (+ (* 3 (** x1 2) a1)) (* 2 y1))
                      x3 (- (** s 2) x1 x2)
                      y3 (- (* s (- x1 x3)) y1)]
                  (Point. x3 y3 a1 b1))))))))))
