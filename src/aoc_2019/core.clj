(ns aoc-2019.core
  (:require [aoc-2019.day-1 :as day-1]
            [aoc-2019.day-2 :as day-2]
            [aoc-2019.day-3 :as day-3]
            [aoc-2019.day-4 :as day-4]
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

(defn ^:private day-2-result
  []
  (build-result
   2
   (day-2/calculate-1202-alarm-state day-2/input)
   (day-2/calculate-output-checksum day-2/input)))

(defn ^:private day-3-result
  []
  (build-result
   3
   (apply day-3/calculate-distance-of-closest-intersection day-3/inputs)
   (apply day-3/calculate-lowest-wire-steps-for-intersection day-3/inputs)))

(defn ^:private day-4-result
  []
  (build-result
   4
   (day-4/calculate-number-of-valid-passwords-in-range day-4/input
                                                       day-4/valid-password?)
   (day-4/calculate-number-of-valid-passwords-in-range day-4/input
                                                       day-4/valid-password-v2?)))

(defn ^:private print-result-table
  []
  (let [results (->> (vector (future (day-1-result))
                             (future (day-2-result))
                             (future (day-3-result))
                             (future (day-4-result)))
                     (mapv deref))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (time
   (print-result-table)))
