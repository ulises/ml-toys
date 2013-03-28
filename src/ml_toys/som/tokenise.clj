(ns ml-toys.som.tokenise
  (:use [clojure.string :only (join lower-case split)]))

(defn tokenise [ sentence ]
  "Splits a sentences on whitespace; removes punctuation and lowercases words."
  (map #(lower-case %)
       (filter #(not (.isEmpty %)) (seq (split #"[\s\.;,-]+" sentence)))))

(defn word-frequencies [ & args ]
  "Counts the frequencies of each word."
  (seq (apply frequencies args)))

(defn to-token [ & words ]
  "Converts words to their keyword equivalents."
  (apply map keyword words))

(defn normalise [ document ]
  (when document
    (let [sum (apply + (vals document))]
      (merge-with (fn [v1 _] (/ v1 sum)) document document))))
