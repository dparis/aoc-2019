(ns aoc-2019.day-3
  (:require [clojure.java.io :as io]
            [clojure.set :as c-set]
            [cuerdas.core :as str]
            [taoensso.tufte :as tt
             :refer [defnp p profiled profile]]))


(tt/add-basic-println-handler! {})

(def inputs
  (-> (io/resource "day_3_input.txt")
      (slurp)
      (str/lines)))

(defnp ^:private parse-direction-entry
  [direction-entry]
  (let [[direction length] (rest (re-find #"([UDLR])(\d+)" direction-entry))]
    (vector direction (Integer. length))))

(defnp ^:private parse-entry-string
  [entry-string]
  (->> (str/split entry-string ",")
       (mapv #(parse-direction-entry (str/strip %)))))

(defnp ^:private interpolate-path-length
  [start length iterate-fn]
  (take (inc length) (iterate iterate-fn start)))

(defnp ^:private interpolate-path-locations
  [starting-location direction length]
  (let [[start-x start-y] starting-location]
    (case direction
      "U"
      (mapv vector
            (repeat start-x)
            (interpolate-path-length start-y length inc))

      "D"
      (mapv vector
            (repeat start-x)
            (interpolate-path-length start-y length dec))

      "L"
      (mapv vector
            (interpolate-path-length start-x length dec)
            (repeat start-y))

      "R"
      (mapv vector
            (interpolate-path-length start-x length inc)
            (repeat start-y)))))

(defnp ^:private directions->path-locations
  [directions]
  (loop [remaining-directions directions
         path                 [[0 0]]]
    (if-let [[cur-direction cur-length] (first remaining-directions)]
      (recur (rest remaining-directions)
             (into (pop path)
                   (interpolate-path-locations (peek path)
                                               cur-direction
                                               cur-length)))
      path)))

(defnp ^:private path-intersections
  [& path-locations]
  (let [location-sets (map set path-locations)]
    (-> (apply c-set/intersection location-sets)
        (disj [0 0]))))

(defnp ^:private manhattan-distance-from-origin
  [location]
  (let [[x y] location]
    (+ (Math/abs x) (Math/abs y))))

(defn calculate-distance-of-closest-intersection
  [& inputs]
  (let [directions     (map parse-entry-string inputs)
        path-locations (map directions->path-locations directions)
        intersections  (apply path-intersections path-locations)]
    (->> (sort-by manhattan-distance-from-origin intersections)
         (first)
         (reduce +))))

(defnp ^:private steps-to-location
  [path-locations location]
  (-> (take-while #(not= location %) path-locations)
      (count)))

(defnp ^:private steps-to-location-v2
  [path-locations location]
  (loop [i                        0
         remaining-path-locations path-locations]
    (let [current-location (first remaining-path-locations)]
      (if-not (= current-location location)
        (recur (inc i) (rest remaining-path-locations))
        i))))

(defnp ^:private steps-to-location-v3
  [path-locations location]
  (.indexOf path-locations location))

(defn ^:private total-steps-for-intersections
  [directions]
  (let [path-locations (map directions->path-locations directions)
        intersections  (apply path-intersections path-locations)]
    (reduce
     (fn [steps intersection]
       (let [total-steps (->> path-locations
                              (map #(steps-to-location-v3 % intersection))
                              (reduce +))]
         (assoc steps intersection total-steps)))
     {}
     intersections)))

(defn calculate-lowest-wire-steps-for-intersection
  [& inputs]
  (let [directions  (map parse-entry-string inputs)
        total-steps (total-steps-for-intersections directions)]
    (-> (sort-by second total-steps)
        first
        second)))
