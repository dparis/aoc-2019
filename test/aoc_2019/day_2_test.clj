(ns aoc-2019.day-2-test
  (:require [aoc-2019.day-2 :as sut
             :refer [execute-program
                     input
                     search-program-input-space]]
            [clojure.test :refer :all]))


(deftest day-2-1
  (testing "execute program"
    (is (= (execute-program [1 0 0 0 99]) [2 0 0 0 99]))
    (is (= (execute-program [2 3 0 3 99]) [2 3 0 6 99]))
    (is (= (execute-program [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
    (is (= (execute-program [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99]))))

(deftest day-2-1
  (testing "search program input space"
    (let [expected-result {:noun   64
                           :verb   72
                           :output 19690720}]
      (is (= (search-program-input-space input 19690720 [0 99] [0 99])
             expected-result)))))
