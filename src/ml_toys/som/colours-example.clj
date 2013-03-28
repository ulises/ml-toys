(ns ml-toys.som.colours-example
  (:use [ml-toys.som.2d :only (scalar-product add-points normalise-to)])
  (:require [ml-toys.som.som :as som])
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener)))

;;; Some constants

(def point-size 10)
(def refresh-rate 150) ; in miliseconds
(def blue (Color. 64 64 255))
(def red (Color. 255 64 64))
(def green (Color. 64 255 64))
(def black (Color. 0 0 0))

(defn colour [ r g b & more ]
  (let [sum (+ r g b)
        r-norm (/ r sum)
        g-norm (/ g sum)
        b-norm (/ b sum)]
    (Color. (int (* r-norm 255)) (int (* g-norm 255)) (int (* b-norm 255)))))

;;; Drawing helpers

(defn point-to-screen-rect [pt]
  (map #(* point-size %) [(int (pt 0)) (int (pt 1)) 1 1]))

(defn fill-point [g pt colour]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g colour)
    (.fillRect g x y width height)))

(defn fill-circle [g pt radius colour]
  (let [[ x y _ _] (point-to-screen-rect pt) ]
  (.setColor g colour)
  (.fillOval g x y radius radius)))

(defn fill-triangle [g pt velocity colour]
  (let [v1 (add-points pt (first (normalise-to 5 velocity)))
        [x y] v1
        v2 [(- x) y]
        v3 [x (- y)]]
    (.setColor g colour)))

;;; Panel, window, etc. stuff


;;; multimethods dont want to work with records, perhaps I can use protocols here?

;;; (defmulti draw (fn [g e] (type e)))

(defn draw [g node]
  (fill-circle g
               (apply vector (vals (:point node)))
               (* point-size 1)
               (apply colour (:weights node))))


(defn som-panel [frame lattice n m]
  (proxy [JPanel ActionListener] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (dorun
                     (map (fn [node] (draw g node)) @lattice)))
    (actionPerformed [e]
                     (.repaint this))
    (getPreferredSize []
                      (Dimension. (* (inc n) point-size)
                                  (* (inc m) point-size)))
    (keyReleased [e])
    (keyTyped [e])))


;;; General set up stuff

(defn new-som-panel [ x y lattice-ref ]
  (let [frame (JFrame. "SOM")
        panel (som-panel frame lattice-ref x y)
        timer (Timer. refresh-rate panel)]
    (doto panel
      (.setFocusable true))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    {:lattice lattice-ref :timer timer}))

(defn run [iter n m]
  (let [lattice (atom (som/new-lattice n m 3))
        som-gui (new-som-panel n m lattice)]
    (som/do-iterations iter lattice som/examples)))