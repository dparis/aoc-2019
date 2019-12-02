(ns aoc-2019.day-1-test
  (:require [aoc-2019.day-1 :as sut
             :refer [module-fuel-requirement
                     partial-fuel-requirement]]
            [clojure.test :refer :all]))

(deftest day-1-1
  (testing "calculate partial fuel requirement"
    (is (= (partial-fuel-requirement 12) 2))
    (is (= (partial-fuel-requirement 14) 2))
    (is (= (partial-fuel-requirement 1969) 654))
    (is (= (partial-fuel-requirement 100756) 33583))))

(deftest day-1-2
  (testing "calculate module fuel requirement"
    (is (= (module-fuel-requirement 14) 2))
    (is (= (module-fuel-requirement 1969) 966))
    (is (= (module-fuel-requirement 100756) 50346))))
