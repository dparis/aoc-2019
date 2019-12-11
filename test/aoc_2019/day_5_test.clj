(ns aoc-2019.day-5-test
  (:require [aoc-2019.day-5 :as sut
             :refer [execute-program!]]
            [clojure.test :refer :all]))


(defn ^:private no-op
  [& args])

(deftest day-5-1
  (testing "arg modes"
    (is (= [1002 4 3 4 99] (execute-program! [1002 4 3 4 33] no-op no-op)))))

(deftest day-5-2
  (testing "op-code 7 - less than"
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 4)]
               (execute-program! [3 9 7 9 10 9 4 9 99 -1 8] input-fn output-fn)
               (last @output_))))
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 8)]
               (execute-program! [3 9 7 9 10 9 4 9 99 -1 8] input-fn output-fn)
               (last @output_))))
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 4)]
               (execute-program! [3 3 1107 -1 8 3 4 3 99] input-fn output-fn)
               (last @output_))))
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 8)]
               (execute-program! [3 3 1107 -1 8 3 4 3 99] input-fn output-fn)
               (last @output_)))))
  (testing "op-code 8 - equals"
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 8)]
               (execute-program! [3 9 8 9 10 9 4 9 99 -1 8] input-fn output-fn)
               (last @output_))))
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 4)]
               (execute-program! [3 9 8 9 10 9 4 9 99 -1 8] input-fn output-fn)
               (last @output_))))
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 8)]
               (execute-program! [3 3 1108 -1 8 3 4 3 99] input-fn output-fn)
               (last @output_))))
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 4)]
               (execute-program! [3 3 1108 -1 8 3 4 3 99] input-fn output-fn)
               (last @output_)))))
  (testing "op-code 5 and 6"
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 0)]
               (execute-program! [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                                 input-fn
                                 output-fn)
               (last @output_))))
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 1)]
               (execute-program! [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                                 input-fn
                                 output-fn)
               (last @output_))))
    (is (= 0 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 0)]
               (execute-program! [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]
                                 input-fn
                                 output-fn)
               (last @output_))))
    (is (= 1 (let [output_   (atom [])
                   output-fn #(swap! output_ conj %)
                   input-fn  (constantly 1)]
               (execute-program! [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]
                                 input-fn
                                 output-fn)
               (last @output_)))))
  (testing "all new opcodes"
    (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
      (is (= 999 (let [output_ (atom [])
                       output-fn #(swap! output_ conj %)
                       input-fn  (constantly 4)]
                   (execute-program! program
                                     input-fn
                                     output-fn)
                   (last @output_))))
      (is (= 1000 (let [output_ (atom [])
                        output-fn #(swap! output_ conj %)
                        input-fn  (constantly 8)]
                    (execute-program! program
                                      input-fn
                                      output-fn)
                    (last @output_))))
      (is (= 1001 (let [output_ (atom [])
                        output-fn #(swap! output_ conj %)
                        input-fn  (constantly 12)]
                    (execute-program! program
                                      input-fn
                                      output-fn)
                    (last @output_)))))))
