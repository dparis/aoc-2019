(ns aoc-2019.day-3-test
  (:require [aoc-2019.day-3 :as sut
             :refer [calculate-distance-of-closest-intersection
                     calculate-lowest-wire-steps-for-intersection]]
            [clojure.test :refer :all]))


(deftest day-3-1
  (testing "calculate distance of closest intersection"
    (is (= (calculate-distance-of-closest-intersection
            "R75,D30,R83,U83,L12,D49,R71,U7,L72"
            "U62,R66,U55,R34,D71,R55,D58,R83")
           159))
    (is (= (calculate-distance-of-closest-intersection
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
           135))))

(deftest day-3-2
  (testing "calculate distance of closest intersection"
    (is (= (calculate-lowest-wire-steps-for-intersection
            "R75,D30,R83,U83,L12,D49,R71,U7,L72"
            "U62,R66,U55,R34,D71,R55,D58,R83")
           610))
    (is (= (calculate-lowest-wire-steps-for-intersection
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
           410))))
