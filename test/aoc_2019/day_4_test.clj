(ns aoc-2019.day-4-test
  (:require [aoc-2019.day-4 :as sut
             :refer [calculate-number-of-valid-passwords-in-range
                     valid-password?
                     valid-password-v2?]]
            [clojure.test :refer :all]))


(deftest day-4-1
  (testing "password validity"
    (is (true? (valid-password? "111111")))
    (is (false? (valid-password? "223450")))
    (is (false? (valid-password? "123789"))))

  (testing "calculate number of valid passwords in range"
    (let [input "245182-790572"]
      (is (= 1099
             (calculate-number-of-valid-passwords-in-range
              input
              valid-password?))))))

(deftest day-4-1
  (testing "password validity"
    (is (true? (valid-password-v2? "112233")))
    (is (true? (valid-password-v2? "111122")))
    (is (false? (valid-password-v2? "123444")))
    (is (false? (valid-password-v2? "111111")))
    (is (false? (valid-password-v2? "223450"))))

  (testing "calculate number of valid passwords in range"
    (let [input "245182-790572"]
      (is (= 710
             (calculate-number-of-valid-passwords-in-range
              input
              valid-password-v2?))))))
