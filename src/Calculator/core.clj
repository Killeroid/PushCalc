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

(defn Int_str
  "Convert string to integer"
  [x]
  (if (string? x)
    (try (Integer/parseInt x)
         (catch Exception e nil))
    nil))


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
                       (let [item (top-item :string state)]
                       (if (or (isoperator? item)
                               (.equals (str item) "_")
                               (.equals (str item) "="))
                         (->> (push-item (top-item :string state) :symbol state)
                              (pop-item :string))
                         state))
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
                     (if (not (empty? (rest (:string state))))
                       (let [x1 (Int_str (stack-ref :string 1 state))
                             x2 (Int_str (stack-ref :string 0 state))]
                         (if (or (not= x1 nil)
                                 (not= x2 nil))
                           (push-item (str x1 x2) :string
                                      (pop-item :string (pop-item :string state)))
                           state))
                       state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Integer instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn negator
  [type]
  (fn [state]
    (if (not (empty? (type state)))
      (let [item (top-item type state)]
        (->> (pop-item type state)
             (push-item (* (abs item) -1) type)))
      state)))

(define-registered integer_neg (negator :integer)) 

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

(define-registered symbol_neg (symb-eqer :symbol '"_"))

(define-registered symbol_answer (symb-eqer :symbol '"="))




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

;(define-registered symbol_call (caller :symbol))


(define-registered symbol_pop (popper :symbol))
(define-registered symbol_dup (duper :symbol))
(define-registered symbol_swap (swapper :symbol))
(define-registered symbol_rot (rotter :symbol))
(define-registered symbol_eq (eqer :symbol))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fitness function helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def buttons (list 0 1 2 3 4 5 6 7 8 9 "+" "-" "*" "/" "_" "=")) ;;The list of buttons on calc

      
(defn init_stack
  "Pushes a string unto the string stack, one character a time"
  [x]
  (let [y (apply str (reverse x))]
    (loop [n (dec (count y)) state (make-push-state)]
      (if (< n 0)
        state
        (recur (dec n) (push-item (str (nth y n)) :string 
                                  (push-item (str (nth y n)) :auxiliary state)))))))


(defn tag-of
  "Returns the tag that refers to x
   Searches the list given and finds
   position of input. The tag is the position * interval"
  [x things]
  (let [interval (round (/ 1000 (count things)))]
    (loop [counter (dec (count things))]
      (if (< counter 0)
        nil
        (if (= (str x) (str (nth things counter)))
          (symbol (str "tagged_" (str (* interval counter))))
          (recur (dec counter)))))))

(defn run-system
  "Runs program and then simulate button presses
   Program is run inbetween each keypress
   Bnk of buttons can be found in things"
  [program things problem]
  (let [size (count problem)]
    (loop [counter 0 state (run-push program (make-push-state))]
      (if (>= counter size)
        (run-push program state)
        (recur (inc counter) (run-push program (push-item (tag-of (nth problem counter) things) 
                                                          :exec state)))))))
  
  
  

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
                "45" 45
                "783023" 783023
                "30-1" 29
                "23*2/2" 23
                "6-4*55" 110
                "21/3*18" 126
                "13/10+5" 6
                "10-1+6*2" 30
                "450*1-1" 449
                "_4*_7" 28})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PushGP call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   

(pushgp
  :error-function (fn [program]
                    (doall
                      (for [input (range (count test_data))]
                        (let [state (run-system program buttons (nth (keys test_data) input))
                              top-num (top-item :integer state)]
                          (if (number? top-num)
                            (abs (- top-num (nth (vals test_data) input)))
                            1000000)))))
  :atom-generators (concat (registered-for-type :symbol)
                           '(integer_add
                              integer_eq
                              integer_neg
                              integer_swap
                              ;integer_yank
                              integer_dup
                              ;integer_yankdup
                              integer_lt
                              ;integer_flush
                              ;integer_shove
                              integer_mult
                              ;integer_stackdepth
                              integer_div
                              integer_gt
                              integer_max
                              ;integer_fromfloat
                              ;integer_fromboolean
                              integer_sub
                              integer_mod
                              integer_rot
                              integer_min
                              ;integer_rand
                              integer_pop)
                           '(boolean_swap
                              boolean_eq
                              ;boolean_yank
                              ;boolean_fromfloat
                              ;boolean_flush
                              boolean_rot
                              boolean_and
                              ;boolean_rand
                              ;boolean_shove
                              boolean_not
                              boolean_or
                              boolean_frominteger
                              ;boolean_stackdepth
                              ;boolean_yankdup
                              boolean_dup
                              boolean_pop)
                           '(;exec_y
                              ;exec_fromziprights
                              exec_pop
                              exec_eq
                              ;exec_stackdepth
                              ;exec_rot
                              ;exec_do*times
                              ;exec_do*count
                              ;exec_s
                              ;exec_do*range
                              ;exec_fromzipnode
                              exec_if
                              exec_when
                              exec_while
                              ;exec_fromziplefts
                              ;exec_fromzipchildren
                              ;exec_k
                              ;exec_yank
                              ;exec_flush
                              ;exec_yankdup
                              ;exec_fromziproot
                              exec_swap
                              exec_dup
                              ;exec_shove
                              exec_noop)
                           '(code_nthcdr
                              code_insert
                              code_fromfloat
                              ;code_stackdepth
                              code_noop
                              code_subst
                              code_overlap
                              ;code_yankdup
                              ;code_fromziprights
                              code_null
                              code_pop
                              code_swap
                              code_append
                              code_member
                              code_do*
                              code_dup
                              code_quote
                              ;code_shove
                              code_cons
                              code_container
                              code_if
                              code_extract
                              code_wrap
                              ;code_fromziproot
                              code_nth
                              code_discrepancy
                              code_size
                              code_length
                              code_cdr
                              code_map
                              ;code_rand
                              code_atom
                              code_contains
                              code_list
                              code_do*range
                              ;code_fromzipnode
                              code_eq
                              ;code_fromzipchildren
                              ;code_flush
                              code_fromboolean
                              ;code_yank
                              code_frominteger
                              code_do*count
                              code_car
                              code_position
                              ;code_fromziplefts
                              code_do
                              code_do*times
                              code_rot)
                           '(string_pop
                              ;string_take
                              string_eq
                              ;string_stackdepth
                              string_map
                              string_rot
                              string_toInt
                              ;string_rand
                              string_concat_Int
                              string_toSymbol
                              ;string_yank
                              string_swap
                              ;string_yankdup
                              ;string_flush
                              ;string_length
                              string_oper?
                              ;string_concat
                              ;string_shove
                              string_dup)
                           (list 'in
                                 (tag-instruction-erc [:exec :integer :boolean :symbol] 1000)
                                 (tagged-instruction-erc 1000)
                                 (tagged-when-instruction-erc 1000)
                                 ))                     
  :max-points 100
  :max-generations 10000
  :reuse-errors false
  :use-single-thread false
  :use-historically-assessed-hardness false
  :dynamically-scaling-genetic-operator-usage true
  :variable-max-points false
  :use-lexicase-selection true)

;(System/exit 0) ;;Comment this line out if you're running this from clooj or an IDE

