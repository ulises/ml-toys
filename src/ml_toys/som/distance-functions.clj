(ns ml-toys.som.distance-functions
  (:require [ml-toys.som.2d :as two-d]
            [ml-toys.som.language-modelling :as lm]
            [ml-toys.som.core :as som-core]))

;;; Euclidean implementation of distances and update-fn

(defn weight-distance [ node other ]
  "Calculates the euclidean distance between a node and an example based on their weighs (n-dim vectors)."
  (let [som-weights (:weights node)
        other-weights (:weights other)]
    (two-d/n-dim-euclidean-distance som-weights other-weights)))

(defn adjust-weights [ node example t & update-labels ]
  "Adjusts the weights of a node when its weight is an n-dim vector. Returns the weights at time t+1"
  (let [node-weights (:weights node)
        example-weights (:weights example)
        updated-weights (two-d/add-points node-weights
                                          (first (two-d/scalar-product (* (som-core/learning-rate t) (theta weight-distance node example t))
                                                                       (two-d/substract-points example-weights node-weights))))
        updated-labels (merge-with + (:labels node) {(:label example) 1})
        {x :x y :y} (:point node)
        new-updated-node (som-node x y updated-weights (:labels node))]
    (if update-labels
      (assoc new-updated-node :labels updated-labels)
      new-updated-node)))


;;; LM weight distance and update-fn

(def P (lm/linear-interpolation lm/P_mle lm/P_coll 0.75))

(defn kl-div-distance [ node other C ]
  "Distance between a som-node and an example using kl-divergence."
  (let [som-document (:document (:weights node))
        other-document (:document (:weights other))
        events (lm/space som-document other-document)
        P_node (fn [ args ] (P (assoc args :document som-document :collection C)))
        P_other (fn [ args ] (P (assoc args :document other-document :collection C)))]
    (lm/kl-divergence P_node P_other events)))

(defn lm-adjust-weights [ node example t & update-labels ]
  "Adjust the language model of a node."
  (let [som-document (:weights node)
        example-document (:weights example)
        updated-weights (tokenise/normalise
                         (merge-with + som-document
                                     (reduce merge {}
                                             (map (fn [[k v]] [k (* (som-core/learning-rate t) (theta weight-distance node example t) v)]) (merge-with (comp abs -) example-document som-document)))))
        {x :x y :y} (:point node)
        updated-labels (merge-with (:labels node) {(:label example) 1})
        updated-node (som-node x y updated-weights (:labels node))]
    (if update-labels
      (assoc updated-node :labels update-labels)
      updated-node)))
