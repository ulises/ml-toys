(ns ml-toys.som.core
  (:require [ml-toys.util :as util]))

(def iterations 500)

(def *sigma_0* 20)  ; 40 / 2 - initial node radius
(def lambda (/ iterations (util/log *sigma_0*)))    ; some random constant
(def l_0 1)       ; initial learning rate

(defrecord SomNode [ point weights labels ])
(defrecord Example [ id label weights ])

(defn som-node [ x y ws labels]
  (SomNode. {:x x :y y} ws labels))

(defn example [ id label ws ]
  (Example. id label ws))

(defn new-ndim-node [ x y dim ]
  (let [rnd-weights (map (fn [ _ ] (rand)) (range dim))]
    (som-node x y (apply vector rnd-weights) {})))

(defn sigma [ t ]
  "radius of any node at time t"
  (* *sigma_0* (util/exp (/ (- t) lambda))))

(defn learning-rate [ t ]
  "learning rate at time t"
  (* l_0 (util/exp (/ (- t) lambda))))

(defn theta [ distance node example t ]
  "weight depending on the distance between a node and an example"
  (let [d (distance node example)
        s (sigma t)]
    (util/exp (- (/ (* d d)
               (* 2 (* s s)))))))