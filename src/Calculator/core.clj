(ns Calculator.core
  (:require [clojush]
            [clojure.contrib.math])
  (:use [clojush]
        [clojure.contrib.math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing Clojush Namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-ns 'clojush)

(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :zip :string :symbol))
(define-push-state-structure)

(defn recognize-literal
  "If thing is a literal, return its type, otherwise return false."
  [thing]
  (cond 
    (integer? thing) :integer
    (number? thing) :float
    (string? thing) :string
    (or (= thing true) (= thing false)) :boolean
    true false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back to calc namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-ns 'Calculator.core)


(defn isoperator?
  "Is x an operator?"
  [x]
  (try (do (apply (ns-resolve *ns* (symbol (str x))) [1 1]) true)
       (catch Exception e false)))

(defn toInt
  "Convert integer to string"
  [x]
  (if (string? x)
    (try (Integer/parseInt x)
         (catch Exception e nil))
    nil))

(defn findargs
  "Find # of arguments x has"
  [x]
  (:arglists (meta x)))


(defn tostr
  "Convert a list of characters to a string"
  [x]
  (apply str x))

(defn type_map
  [type]
  (fn [state]
    (if (not (or (empty? (:code (push-item (ensure-list (type state)) :code state)))
                 (empty? (:exec state))))
      (let [answer (push-item (concat
                                (doall (for [item (ensure-list (first (:code state)))]
                                         (list 'code_quote
                                               item
                                               (first (:exec state)))))
                                '(code_wrap)
                                (doall (for [item (rest (ensure-list (first (:code state))))]
                                         'code_cons)))
                              :exec
                              (pop-item :code (pop-item :exec state)))]
        (pop-item :code (push-item (top-item :code answer) :exec answer)) 
        state))))
(define-registered string_map (type_map :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;String instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-registered string_oper?
  (fn [state]
    (if (not (empty? (:string state)))
      (push-item (isoperator? (top-item :string state)) :boolean state)
      state)))

(define-registered string_toSymbol
                   (fn [state]
                     (if (not (empty? (:string state)))
                       (if (isoperator? (top-item :string state))
                         (->> (push-item (top-item :string state) :symbol state)
                              (pop-item :string))
                         state)
                       state)))
                     
(define-registered string_toInt
                   (fn [state]
                     (if (not (empty? (:string state)))
                       (try (push-item (Integer/parseInt (top-item :string state)) 
                                       :integer state)
                            (catch Exception e state))
                       state)))

(define-registered string_concat_Int
                   (fn [state]
                     (let [x1 (toInt (stack-ref :string 1 state))
                           x2 (toInt (stack-ref :string 1 state))]
                       (if (or (not= x1 nil)
                               (not= x2 nil))
                         (push-item (str x1 x2) :string
                                    (pop-item :string (pop-item :string state)))
                         state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn symb-eqer
  "Is symbol equal to operator"
  [type second]
  (fn [state]
    (if (not (empty? (type state)))
      (let [first (top-item type state)]
        (->> (pop-item type state)
             (push-item (.equals (str first) (str second)) :boolean)))
      state)))

(define-registered symbol_plus (symb-eqer :symbol '+))

(define-registered symbol_sub (symb-eqer :symbol '-))

(define-registered symbol_div (symb-eqer :symbol '/))

(define-registered symbol_mult (symb-eqer :symbol '*))

(define-registered symbol_pop (popper :symbol))

(defn caller
  "apply type to top 2 items on integer stack"
  [type]
  (fn [state]
    (if (not (empty? (rest (:integer state))))
      (if (not (empty? (type state)))
        (let [t1 (top-item type state)
              i1 (stack-ref :integer 1 state)
              i2 (stack-ref :integer 0 state)] 
          (push-item (apply (ns-resolve *ns* (symbol (str t1))) [i1 i2]) 
                     :integer state))
        state)
      state)))

(define-registered symbol_call (caller :symbol))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fitness function helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init_stack
  "Pushes a string unto the string stack, one character a time"
  [x]
  (loop [n (dec (count x)) state (make-push-state)]
    (if (< n 0)
      state
      (recur (dec n) (push-item (str (nth x n)) :string 
                                (push-item (str (nth x n)) :auxiliary state))))))

(define-registered in 
                   (fn [state]
                     (let [x (:auxiliary state)] 
                       (loop [n (dec (count x)) stata state]
                         (if (< n 0)
                           stata
                           (recur (dec n) 
                                  (push-item (str (nth x n)) :string stata)))))))

                 

(def test_data {"2*3" 6
                "5+9" 14
                "8/2" 4
                "30-1" 29})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PushGP call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   
(pushgp
  :error-function (fn [program]
                    (doall
                      (for [input (range 0 (count test_data))]
                        (let [state (run-push program
                                              (init_stack (nth (keys test_data) input)))
                              top-num (top-item :integer state)]
                          (if (number? top-num)
                            (abs (- top-num (nth (vals test_data) input)))
                            100)))))
  :atom-generators (concat (registered-for-type :symbol)
                           (registered-for-type :integer)
                           ;(registered-for-type :string)
                           (list 'in
                                 'string_oper?
                                 'string_toSymbol
                                 'string_toInt
                                 'string_concat_Int
                                 'string_map
                                 (tag-instruction-erc [:exec :integer :symbol] 1000)
                                 (tagged-instruction-erc 1000)))                     
  :max-points 100
  :max-generations 100000)

(System/exit 0)
