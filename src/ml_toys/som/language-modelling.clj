(ns ml-toys.som.language-modelling
  (:require [clojure [set :as s]]
            [ml-toys.util :as util]))


(defn document [ & args ]
  "Builds a document from the args. A document is a map where the keys are the terms and the values the occurrences of each term in the document. Position is ignored in this document model."
  (apply merge {} args))

(defn collection [& args]
  "Builds a collection from a list of documents. Frequencies are added together."
  (apply merge-with + {} args))

(defn length [ document ]
  "Calculates the document length as the addition of the number of times each term has appeared in the document"
  (apply + (vals document)))

(defn P_mle [ {w :w document :document :as args} ]
  "Calculates P(w|document) as the maximum-likelihood estimate."
  (if (document w)
    (/ (document w) (length document))
    0))

(defn P_coll [ {w :w C :collection :as args} ]
  "Calculates P(w|C) as the maximum likelihood estimate"
  (let [size (length C)]
    (if (C w)
      (/ (C w) size)
      (/ 1.0 (inc size)))))

(defn linear-interpolation [ f g lambda ]
  "Linear interpolation of the functions f and g defined as: lambda * f + (1 - lambda) g"
  (fn [& args]
    (+ (* lambda (apply f args)) (* (- 1 lambda) (apply g args)))))

(defn P_unif [ dim ]
  "Uniform distribution over a space of n events."
  (fn [& args] (/ 1 dim)))

(defn kl-divergence [ P G space ]
  "Calculates the Kullback-Leibler divergence KL(P||G) of P and G on the space of events."
  (let [P_e (map P space)
        G_e (map G space)]
    (- (reduce + (map * P_e (map util/log P_e))) (reduce + (map * P_e (map log G_e))))))

  ;; (reduce +
  ;;         (map (fn [ e ] (* (P e) (log (/ (P e) (G e))))) space)))

(defn lambda-divergence [ P G space lambda ]
  "See http://en.wikipedia.org/wiki/Kullback Leibler_divergence#Symmetrised_divergence"
  (let [lint (linear-interpolation P G lambda)]
    (+ (* lambda (kl-divergence P lint space))
       (* (- 1 lambda) (kl-divergence G lint space)))))

(defn js-divergence [ P G space ]
  "Jensen-Shannon divergence"
  (lambda-divergence P G space 1/2))

(defn space [ & documents ]
  (when documents
    (set (map (fn [e] {:w e}) (keys (apply merge documents))))))

(defn example [ d C ]
  {:document d :space (space d) :P (fn [ args ] ())})
