(ns ml-toys.util)

(defn bound-rand
  "Returns a random number between min and max"
  [min max]
  (+ (rand (- max min)) min))

(defn make-random-uniform-vector
  "Creates a random uniform vector of dimension d and boundaries min and max"
  [ d min max ]
  (map (fn [_] (bound-rand min max)) (range d)))

(defn abs [x] (Math/abs x))
(defn sqr [x] (* x x))
(defn sqrt [x] (Math/sqrt x))
(defn exp [x] (Math/exp x))
(defn log [x] (Math/log x))
