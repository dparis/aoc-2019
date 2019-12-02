(ns aoc-2019.day-1
  (:require [clojure.java.io :as io]
            [cuerdas.core :as str]))

(def input
  (->> (io/resource "day_1_input.txt")
       (slurp)
       (str/lines)
       (map #(Integer. %))))

(defn partial-fuel-requirement
  [mass]
  (-> (/ mass 3)
      (Math/floor)
      (int)
      (- 2)))

(defn module-fuel-requirement
  [mass]
  (loop [fuel-parts   []
         current-mass mass]
    (let [fuel-required (partial-fuel-requirement current-mass)]
      (if (> fuel-required 0)
        (recur (conj fuel-parts fuel-required)
               fuel-required)
        (reduce + fuel-parts)))))

(defn calculate-module-fuel-requirement
  [input]
  (->> (mapv partial-fuel-requirement input)
       (reduce +)))

(defn calculate-total-fuel-requirement
  [input]
  (->> (mapv module-fuel-requirement input)
       (reduce +)))
