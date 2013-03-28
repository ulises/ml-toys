(ns ml-toys.pso
  (:require [ml-toys.util :as util]))

;;; This is a simple implementation of the Particle Swarm Optimisation algorithm
;;; and as such it's just a toy.

(defn make-particle
  "Creates a new candidate solution"
  ([ position best velocity ]
     {:position position :best best :velocity velocity })
  ([ position velocity ]
     (make-particle position position velocity))
  ([]
     (make-particle [] [] [])))

(defn make-random-particle
  "Creates a new candidate solution with random position and velocity.
   Position and velocity are vectors in R^(len boundaries). boundaries
   determines the search space and is a seq of pairs [min max]"
  ([ dim min max ]
     (make-particle
      (util/make-random-uniform-vector dim min max)
      (util/make-random-uniform-vector dim min max)
      (util/make-random-uniform-vector dim min max)))
  ([]
     (make-random-particle 2 0 1)))

(defn position
  "Returns the position of a particle"
  [ particle ]
  (:position particle))

(defn velocity
  "Returns the velocity of a particle"
  [ particle ]
  (:velocity particle))

(defn best-position
  "Returns the best position for a particle"
  [ particle ]
  (:best particle))

(defn update-position
  "Updates the position of a particle"
  [ particle new-position ]
  (make-particle
   new-position
   (best-position particle)
   (velocity particle)))

(defn update-best-position
  "Updates the best position of a particle according to the fitness function f"
  [ particle f ]
  (let [current-position (position particle)
        best-current-position (best-position particle)
        current-fitness (f current-position)
        best-fitness (f best-current-position)]
    (make-particle (position particle)
                   (if (< best-fitness current-fitness)
                     best-current-position
                     current-position)
                   (velocity particle))))

(defn update-velocity
  "Updates the velocity at which a particle is moving"
  [ particle new-velocity ]
  (make-particle (position particle)
                 (best-position particle)
                 new-velocity))

;;; next velocity of particle:
;;; v_t+1 = v_t + gamma_p * rnd[0 1] (best p of particle - x_t) +
;;;         gamma_g * rnd[0 1] (best particle of swarm - x_t)
(defn next-velocity
  "Calculates the next velocity of a particle"
  [ particle best-particle gamma_p gamma_g omega ]
  (let [current-velocity (velocity particle)
        diff-particle-best (map - (best-position particle)
                                (position particle))
        diff-global-best (map - (position best-particle)
                              (position particle))]
    (map + (map * (repeat omega) current-velocity)
         (map * (repeat gamma_p) (repeatedly rand) diff-particle-best)
         (map * (repeat gamma_g) (repeatedly rand) diff-global-best))))

;;; next position of particle: x_t+1 = x_t + v_t+1
(defn next-position
  "Calculates the next position of a particle according to its own velocity"
  [ particle ]
  (map + (position particle) (velocity particle)))

(defn best-particle
  "Picks out the best particle in a swarm according to the fitness function f"
  [ swarm f ]
  (when swarm
    (let [evaled-swarm
          (map (fn
                 [ particle ]
                 {:fitness (f (position particle)) :particle particle})
               swarm)]
      (:particle (first (sort-by :fitness evaled-swarm))))))

(defn epoch
  "An iteration of the PSO algorithm. It involves updating all particles in
  the swarm using the gammas, which control the influence of the social and
  global behaviour factors, and updating the best particle in the swarm
  according to the fitness function f."
  ([ swarm best-particle gamma_p gamma_g omega f updated-particles ]
     (loop [still-to-update swarm
            updated-particles updated-particles]
       (let [particle (first still-to-update)
             new-velocity (next-velocity particle best-particle gamma_p gamma_g omega)
             particle-with-new-velocity (update-velocity particle new-velocity)
             new-position (next-position particle-with-new-velocity)
             particle-with-new-position (update-position particle-with-new-velocity
                                                         new-position)]
         (if (empty? still-to-update)
           updated-particles
           (recur (next still-to-update)
                  (conj updated-particles (update-best-position particle-with-new-position
                                                                f)))))))
  ([ swarm best-particle gamma_p gamma_g f omega ]
     (epoch swarm best-particle gamma_p gamma_g f omega [])))

(defn pso-iter [n initial-swarm gamma-p gamma-g omega f ]
  "Perform a series of iterations (epochs) of the PSO algorithm."
  (loop [iter n
         swarm initial-swarm
         best (best-particle swarm f)]
    (if (pos? iter)
      (let [new-swarm (epoch swarm best gamma-p gamma-g omega f)]
        (recur (dec iter) new-swarm (best-particle new-swarm f)))
      (best-position (best-particle swarm f)))))

(defn make-random-swarm
  [size dim min max]
  (take size (repeatedly (fn [] (make-random-particle dim min max)))))

(defn example [iters]
  (let [swarm (make-random-swarm 20 2 -10 10)
        vector-norm (fn [[x y]] (Math/abs (dec (Math/sqrt (+ (* x x) (* y y))))))
        gamma-p 0.1
        gamma-g 0.1
        omega 0.1
        solution (pso-iter iters swarm gamma-p gamma-g omega vector-norm)]
    [solution (vector-norm solution)]))