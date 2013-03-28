(ns ml-toys.som.2d
  (:require [ml-toys.util :as util]))

(defn add-points [& pts]
  (vec (apply map + pts)))
(defn substract-points [& pts]
  (vec (apply map - pts)))


(defn scalar-product [scalar & pts]
  "Product between a scalar and 2D vectors"
  (letfn [(single-scalar-product [ s v ]
                                 (vec (map (partial * s) v)))]
    (vec (map (partial single-scalar-product scalar) pts))))

(defn n-dim-euclidean-distance
  "Traditional euclidean distance in the n-dimensional space"
  [pt other]
  (util/sqrt
   (reduce +
           (map
            (fn [ p ] (* p p))
            (substract-points pt other)))))

(defn euclidean-distance
  "Traditional euclidean distance"
  [ p1 p2 ]
  (n-dim-euclidean-distance p1 p2))

(defn magnitude [& pts]
  (map (fn [ p ] (euclidean-distance (repeat (count p) 0) p)) pts))

(defn normalise-to [ desired-magnitude & pts ]
  "Normalises 2D vectors to a desired magnitude"
  (map (fn [ v ] (first (scalar-product (/ desired-magnitude (first (magnitude v))) v)))
       pts))

(defn normalise [& pts]
  "Normalises 2D vectors"
  (apply normalise-to 1 pts))
  ;; (map (fn [ v ] (first (scalar-product (/ 1 (first (magnitude v))) v)))
  ;;      pts))

(defn average [& pts]
  "Averages a sequence of 2D points"
  (first (scalar-product (/ 1 (count pts))
                  (apply add-points pts))))

(defn cap-to [ [ f1 f2 ] [ m1 m2 ] ]
  (letfn [(within-range [n min max]
                          (cond (> n max) max
        (< n min) min
        :default n))]
    [(within-range f1 (- m1) m1) (within-range f2 (- m2) m2)]))

(defn cap-magnitude-to [ v max-magnitude ]
  (let [v-magnitude (first (magnitude v))]
    (if (> v-magnitude max-magnitude)
      (first (scalar-product max-magnitude
                             (first (normalise v))))
      v)))
