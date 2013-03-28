(ns ml-toys.som.som
  (:require [ml-toys.som.2d :as two-d]
            [ml-toys.util :as util]
            [ml-toys.som.language-modelling :as lm]
            [ml-toys.som.tokenise :as tokenise]
            [ml-toys.som.distance-functions :as d-fns]
            [ml-toys.som.core :as som-core]))

;;; General SOM algorithm

(defn point-distance [ node other ]
  "Calculates the euclidean distance between the [x  y] positions of two  nodes (2D vectors)"
  (let [som-point (vals (:point node))
        other-point (vals (:point other))]
    (two-d/n-dim-euclidean-distance som-point other-point)))

(defn score-nodes-by [ f lattice ]
  "Scores all the nodes in the lattice according to a score function f"
  (map (fn [ node ]
         {:score (f node)
          :node node})
       lattice))

(defn closest-node [ distance-fn lattice example ]
  "Returns the closest node according to its distance to the example"
  (:node
   (first
    (sort-by :score
             (score-nodes-by (partial distance-fn example)
                             lattice)))))

(defn neighbours [ distance-fn lattice node t ]
  "Tags all the som nodes within the radius of action of a given node at time t"
  (split-with
   (fn [p] (< (:score p) (som-core/sigma t)))
   (sort-by :score
            (score-nodes-by (partial distance-fn node)
                            lattice))))

(defn single-run [ update-fn lattice example t ]
  (let [best-matching-unit (closest-node d-fns/weight-distance lattice example)
        [ s1 s2 ] (neighbours point-distance lattice best-matching-unit t)
        nodes-to-update (map :node s1)
        other-nodes (map :node s2)]
    (concat (map (fn [node] (update-fn node example t :true)) nodes-to-update) other-nodes)))

(defn epoch [ lattice examples t]
  (if (seq examples)
    (recur (single-run d-fns/adjust-weights lattice (first examples) t) (rest examples) t)
    lattice))

(defn new-lattice [n m dim]
  (alter-var-root #'som-core/*sigma_0* (fn [ _ ] (/ (max n m) 2)))
  (apply vector
         (for [x (range n) y (range m)] (som-core/new-ndim-node x y dim))))

(def examples [(som-core/example 1 :red [1 0 0]) (som-core/example 2 :green [0 1 0]) (som-core/example 3 :blue [0 0 1])
               (som-core/example 4 :red-green [1 1 0]) (som-core/example 5 :green-blue [0 1 1]) (som-core/example 6 :red-blue [1 0 1])])

(defn do-iterations
  ([ end lattice examples ]
     (do-iterations 0 end lattice examples))
  ([ start end lattice examples ]
     (last
      (map
       (fn [ t ] (swap! lattice epoch (shuffle examples) t) nil)
       (range start end)))
     nil))

(defn reset-lattice [ lattice n m dim ]
  (swap! lattice (fn [_] (new-lattice n m dim))))

(defn predict [ lattice example ]
  (let [bmu (closest-node lattice example)]
    (into {}
          (reverse (sort-by second (:labels bmu))))))
