(ns ml-toys.knn
  (:require [ml-toys.util :as util]))

(defn weighted-euclidean
  "Weighted euclidean distance defined as: d(x, y, w) = sqrt sum_i w_i * (x_i - y_i)^2"
  [weights]
  (fn [x y]
    (util/sqrt
     (reduce +
             (map * weights
                  (map util/sqr (map - x y)))))))

(defn knn-distance
  "Converts a distance function a distance function on the space of examples from data."
  [d]
  (fn [item1 item2]
    (d (:features item1) (:features item2))))

(defn knn
  "Builds a kNN model of k neighbours over the space of train and using as distance function d."
  [train k d]
  (fn [item] (take k (sort-by #(d item %) train))))

(defn knn-classify
  "Given a list of the n nearest neighbours, it takes a voting (not weighted) and selects the winner class."
  [neighbours]
  (first
   (first
    (sort-by (fn [[klass neighbours]]
               (- (count neighbours)))
             (group-by :class neighbours)))))

(defn evaluate-predicted
  "Compares a predicted label vs. a true label. This is the classification cost function where the penalty of missclassifying any example carries equal weight."
  [predicted correct]
  (if (= predicted correct) 0 1))

(defn classification-error
  "Calculates the classification error of a model over a dataset."
  [model dataset]
  (let [predicted (map (comp knn-classify model) dataset)]
    (/ (reduce + (map evaluate-predicted predicted (map :class dataset))) (count dataset))))

(defn knn-fitness
  "Builds a fitness function suitable to be learnt using PSO based on the kNN algorithm."
  [train k test]
  (fn [particle]
    (let [d (knn-distance (weighted-euclidean (:position particle)))
          model (knn train k d)]
      (classification-error model test))))
