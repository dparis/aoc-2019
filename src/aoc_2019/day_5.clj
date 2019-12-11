(ns aoc-2019.day-5
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cuerdas.core :as str]))


(def input
  (-> (io/resource "day_5_input.edn")
      (slurp)
      (edn/read-string)))

(defn ^:private update-program-with-inputs
  [program noun verb]
  (-> (vec program)
      (assoc 1 noun)
      (assoc 2 verb)))

(defn ^:private arg-value
  [program op-context arg-index]
  (let [arg-modes (:arg-modes op-context)
        op-inputs (:op-inputs op-context)]
    (if (= 0 (nth arg-modes arg-index))
      (nth program (nth op-inputs arg-index))
      (nth op-inputs arg-index))))

(defn ^:private execute-op-code-1
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        operand-1    (arg-value-fn 0)
        operand-2    (arg-value-fn 1)]
    (assoc program
           (get-in op-context [:op-inputs 2])
           (+ operand-1 operand-2))))

(defn ^:private execute-op-code-2
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        operand-1    (arg-value-fn 0)
        operand-2    (arg-value-fn 1)]
    (assoc program
           (get-in op-context [:op-inputs 2])
           (* operand-1 operand-2))))

(defn ^:private execute-op-code-3
  [program op-context input-fn]
  (assoc program (get-in op-context [:op-inputs 0]) (input-fn)))

(defn ^:private execute-op-code-4
  [program op-context output-fn]
  (let [arg-value-fn (partial arg-value program op-context)
        output-value (arg-value-fn 0)]
    (output-fn output-value)
    program))

(defn ^:private execute-op-code-5
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        test-value   (arg-value-fn 0)
        jump-to      (arg-value-fn 1)]
    (when-not (= 0 test-value)
      jump-to)))

(defn ^:private execute-op-code-6
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        test-value   (arg-value-fn 0)
        jump-to      (arg-value-fn 1)]
    (when (= 0 test-value)
      jump-to)))

(defn ^:private execute-op-code-7
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        operand-1    (arg-value-fn 0)
        operand-2    (arg-value-fn 1)
        output-index (get-in op-context [:op-inputs 2])]
    (if (< operand-1 operand-2)
      (assoc program output-index 1)
      (assoc program output-index 0))))

(defn ^:private execute-op-code-8
  [program op-context]
  (let [arg-value-fn (partial arg-value program op-context)
        operand-1    (arg-value-fn 0)
        operand-2    (arg-value-fn 1)
        output-index (get-in op-context [:op-inputs 2])]
    (if (= operand-1 operand-2)
      (assoc program output-index 1)
      (assoc program output-index 0))))

(def ^:private op-code-arity
  {1  3
   2  3
   3  1
   4  1
   5  2
   6  2
   7  3
   8  3
   99 0})

(defn ^:private parse-op-context
  [program current-instruction-pointer]
  (let [raw-op-code   (nth program current-instruction-pointer)
        op-code-parts (as-> raw-op-code %
                        (str %)
                        (str/pad % {:length  5
                                    :padding "0"})
                        (re-find #"([01])([01])([01])(\d{2})" %)
                        (rest %)
                        (mapv str/parse-int %))
        op-code       (last op-code-parts)
        op-arity      (get op-code-arity op-code)
        op-inputs     (->> (inc current-instruction-pointer)
                           (subvec program)
                           (take op-arity)
                           (vec))
        arg-modes     (vec (reverse (take 3 op-code-parts)))
        next-ip       (when-not (= 99 op-code)
                        (->> (get op-code-arity op-code)
                             (+ current-instruction-pointer)
                             (inc)))]
    {:op-code                  op-code
     :op-arity                 op-arity
     :op-inputs                op-inputs
     :arg-modes                arg-modes
     :next-instruction-pointer next-ip}))

(defn execute-program!
  ([program noun verb input-fn output-fn]
   (let [updated-program (update-program-with-inputs program noun verb)]
     (execute-program! updated-program input-fn output-fn)))
  ([program input-fn output-fn]
   (loop [current-program             (vec program)
          current-instruction-pointer 0]
     (let [op-context               (parse-op-context current-program
                                                      current-instruction-pointer)
           op-code                  (:op-code op-context)
           next-instruction-pointer (:next-instruction-pointer op-context)]
       (case op-code
         ;; OP-CODE 1 - ADD
         ;; ARG-1 - ARG INPUT OF FIRST OPERAND
         ;; ARG-2 - ARG INPUT OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         1
         (recur (execute-op-code-1 current-program op-context)
                next-instruction-pointer)

         ;; OP-CODE 2 - MULTIPLY
         ;; ARG-1 - ARG INPUT OF FIRST OPERAND
         ;; ARG-2 - ARG INPUT OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         2
         (recur (execute-op-code-2 current-program op-context)
                next-instruction-pointer)

         ;; OP-CODE 3 - INPUT
         ;; ARG-1 - INDEX TO SAVE INPUT INTEGER
         3
         (recur (execute-op-code-3 current-program op-context input-fn)
                next-instruction-pointer)

         ;; OP-CODE 4 - INPUT
         ;; ARG-1 - ARG INPUT TO OUTPUT
         4
         (recur (execute-op-code-4 current-program op-context output-fn)
                next-instruction-pointer)


         ;; OP-CODE 5 - JUMP-IF-TRUE
         ;; ARG-1 - ARG TO TEST
         ;; ARG-2 - INDEX TO JUMP TO
         5
         (let [jip (execute-op-code-5 current-program op-context)]
           (recur current-program (or jip next-instruction-pointer)))

         ;; OP-CODE 6 - JUMP-IF-NOT-TRUE
         ;; ARG-1 - ARG TO TEST
         ;; ARG-2 - INDEX TO JUMP TO
         6
         (let [jip (execute-op-code-6 current-program op-context)]
           (recur current-program (or jip next-instruction-pointer)))

         ;; OP-CODE 7 - LESS THAN
         ;; ARG-1 - ARG INPUT OF FIRST OPERAND
         ;; ARG-2 - ARG INPUT OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         7
         (recur (execute-op-code-7 current-program op-context)
                next-instruction-pointer)

         ;; OP-CODE 8 - EQUALS
         ;; ARG-1 - ARG INPUT OF FIRST OPERAND
         ;; ARG-2 - ARG INPUT OF SECOND OPERAND
         ;; ARG-3 - INDEX OF OUTPUT
         8
         (recur (execute-op-code-8 current-program op-context)
                next-instruction-pointer)

         ;; OP-CODE 99 - TERMINATE
         99
         current-program

         ;; ELSE
         (throw (ex-info "UNSUPPORTED OP-CODE"
                         (assoc op-context :program current-program))))))))

(defn calculate-diagnostic-code-1
  [input]
  (let [output_ (atom [])]
    (execute-program! input (constantly 1) #(swap! output_ conj %))
    (last @output_)))

(defn calculate-diagnostic-code-5
  [input]
  (let [output_ (atom [])]
    (execute-program! input (constantly 5) #(swap! output_ conj %))
    (last @output_)))
