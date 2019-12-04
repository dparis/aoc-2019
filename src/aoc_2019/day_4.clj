(ns aoc-2019.day-4
  (:require [cuerdas.core :as str]))


(def input
  "245182-790572")

(defn ^:private parse-input
  [input]
  (->> (str/split input "-")
       (mapv str/strip)))

(defn ^:private split-digits
  [n]
  (map str/parse-int (str/split (str n) "")))

(defn ^:private password-lower-bound
  [range-bottom-str]
  (let [digits (split-digits range-bottom-str)]
    (loop [lower-bound-digits     (vector (first digits))
           remaining-range-digits (rest digits)]
      (if (<= (last lower-bound-digits) (first remaining-range-digits))
        (recur (conj lower-bound-digits (first remaining-range-digits))
               (rest remaining-range-digits))
        (-> (into lower-bound-digits
                  (repeat (count remaining-range-digits)
                          (last lower-bound-digits)))
            (str/join)
            (Integer.))))))

(defn ^:private password-upper-bound
  [range-top-str]
  (let [digits (split-digits range-top-str)]
    (loop [upper-bound-digits     (vector (first digits))
           remaining-range-digits (rest digits)]
      (if (<= (last upper-bound-digits) (first remaining-range-digits))
        (recur (conj upper-bound-digits (first remaining-range-digits))
               (rest remaining-range-digits))
        (-> (into (pop upper-bound-digits)
                  (into (vector (dec (last upper-bound-digits)))
                        (repeat (count remaining-range-digits) 9)))
            (str/join)
            (Integer.))))))

(defn valid-password?
  [password]
  (let [digits (split-digits password)]
    (boolean
     (when (= 6 (count digits))
       (loop [current-digit    (first digits)
              remaining-digits (rest digits)
              double-found?    false]
         (if-let [next-digit (first remaining-digits)]
           (when (<= current-digit next-digit)
             (recur next-digit
                    (rest remaining-digits)
                    (or double-found? (= current-digit next-digit))))
           double-found?))))))

(defn valid-password-v2?
  [password]
  (let [digits (split-digits password)]
    (boolean
     (when (= 6 (count digits))
       (loop [current-digit    (first digits)
              remaining-digits (rest digits)
              previous-digit   nil
              double-found?    false]
         (if-let [next-digit (first remaining-digits)]
           (when (and next-digit (<= current-digit next-digit))
             (let [nnext-digit (second remaining-digits)]
               (recur next-digit
                      (rest remaining-digits)
                      current-digit
                      (or double-found? (and (= current-digit next-digit)
                                             (not= current-digit nnext-digit)
                                             (not= previous-digit current-digit))))))
           double-found?))))))

(defn calculate-number-of-valid-passwords-in-range
  [input valid-password-fn]
  (let [[range-lower-str range-upper-str] (parse-input input)
        lower-bound                       (password-lower-bound range-lower-str)
        upper-bound                       (password-upper-bound range-upper-str)]
    (->> (range lower-bound (inc upper-bound))
         (filter valid-password-fn)
         (count))))
