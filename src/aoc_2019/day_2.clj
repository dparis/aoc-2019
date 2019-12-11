(ns aoc-2019.day-2
  (:require [clojure.math.combinatorics :as cmc]))


(def input
  [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 10 19 2 9 19 23 2 23 10 27 1 6 27 31
   1 31 6 35 2 35 10 39 1 39 5 43 2 6 43 47 2 47 10 51 1 51 6 55 1 55 6 59 1
   9 59 63 1 63 9 67 1 67 6 71 2 71 13 75 1 75 5 79 1 79 9 83 2 6 83 87 1 87
   5 91 2 6 91 95 1 95 9 99 2 6 99 103 1 5 103 107 1 6 107 111 1 111 10 115 2
   115 13 119 1 119 6 123 1 123 2 127 1 127 5 0 99 2 14 0 0])

(defn ^:private update-program-with-inputs
  [program noun verb]
  (-> (vec program)
      (assoc 1 noun)
      (assoc 2 verb)))

(defn ^:private execute-opcode-1
  [program arg-1 arg-2 arg-3]
  (let [operand-1 (nth program arg-1)
        operand-2 (nth program arg-2)]
    (assoc program arg-3 (+ operand-1 operand-2))))

(defn ^:private execute-opcode-2
  [program arg-1 arg-2 arg-3]
  (let [operand-1 (nth program arg-1)
        operand-2 (nth program arg-2)]
    (assoc program arg-3 (* operand-1 operand-2))))

(defn execute-program
  ([program noun verb]
   (let [updated-program (update-program-with-inputs program noun verb)]
     (execute-program updated-program)))
  ([program]
   (loop [current-program         (vec program)
          current-instruction-ptr 0]
     (let [[opcode arg-1 arg-2 arg-3] (->> current-instruction-ptr
                                           (subvec current-program)
                                           (take 4))
           next-instruction-ptr       (+ 4 current-instruction-ptr)]
       (case opcode
         ;; OPCODE 1 - ADD
         ;; ARG-1 - INDEX OF FIRST OPERAND
         ;; ARG-2 - INDEX OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         1
         (recur (execute-opcode-1 current-program arg-1 arg-2 arg-3)
                next-instruction-ptr)

         ;; OPCODE 2 - MULTIPLY
         ;; ARG-1 - INDEX OF FIRST OPERAND
         ;; ARG-2 - INDEX OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         2
         (recur (execute-opcode-2 current-program arg-1 arg-2 arg-3)
                next-instruction-ptr)

         ;; OPCODE 99 - TERMINATE
         99
         current-program

         ;; ELSE
         (throw (ex-info "UNSUPPORTED OPCODE"
                         {:opcode          opcode
                          :arg-1           arg-1
                          :arg-2           arg-2
                          :arg-3           arg-3
                          :program         current-program
                          :instruction-ptr current-instruction-ptr})))))))

(defn search-program-input-space
  [program target noun-min-max verb-min-max]
  (let [[noun-min noun-max] noun-min-max
        [verb-min verb-max] verb-min-max
        inputs              (cmc/cartesian-product
                             (range noun-min (inc noun-max))
                             (range verb-min (inc verb-max)))
        execute-fn          (fn [[noun verb]]
                              (let [result (execute-program program noun verb)]
                                {:noun   noun
                                 :verb   verb
                                 :output (first result)}))]
    (->> (map execute-fn inputs)
         (filter #(= (get % :output) target))
         (first))))

(defn calculate-1202-alarm-state
  [input]
  (-> (execute-program input 12 2)
      (first)))

(defn calculate-output-checksum
  [input]
  (when-let [result (search-program-input-space input 19690720 [0 99] [0 99])]
    (+ (* 100 (:noun result)) (:verb result))))
