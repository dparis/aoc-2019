(ns aoc-2019.core
  (:require [aoc-2019.day-1 :as day-1]
            [clojure.pprint :as pp])
  (:gen-class))

(defn ^:private build-result
  [day first-answer second-answer]
  {"Day"                day
   "First Star Answer"  first-answer
   "Second Star Answer" second-answer})

(defn ^:private day-1-result
  []
  (build-result
   1
   (day-1/calculate-module-fuel-requirement day-1/input)
   (day-1/calculate-total-fuel-requirement day-1/input)))

(defn ^:private print-result-table
  []
  (let [results (->> (vector (future (day-1-result)))
                     (mapv deref))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (time
   (print-result-table)))
