;; Thanks to Lee Spector for providing the foundation for this genetic
;; programming algorithm. The original content can be found here:
;; https://gist.github.com/lspector/1297325 

(ns uber.core
  (:use [clojure.core.match :only (match)])
  (:import org.nlogo.headless.HeadlessWorkspace)
  (:gen-class :main true))

(def ticks 500)
(def surge-income 4)

(def workspace
  (doto (HeadlessWorkspace/newInstance)
    (.open "models/small.nlogo")))

(defrecord gene [error expr])

(def terminals
  (list 'true 'false 'ncount 'edist 'dist))

(defn terminal?
  [t]
  (if (seq? t)
    (if (or (= 'ncount (first t)) (= 'dist (first t)))
      true)
    true))

(defn random-function
  []
  (rand-nth '(and or not)))

(defn random-terminal
  []
  (let [x (rand-nth terminals)]
    (if (= x 'ncount)
      `(~'ncount ~(rand-int 8) ) ; ~' to strip ns qualifiers
      (if (= x 'dist)
        `(~'dist ~(/ (rand-int 10) 10.0))
        x))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [operator (random-function)]
      (if (= operator 'not)
        (list operator
              (random-code (dec depth)))
        (list operator
              (random-code (dec depth))
              (random-code (dec depth)))))))


(defn parse 
  [expr]
  (if (terminal? expr)
    (str expr)
    (let [oper (first expr)
          op1 (second expr)
          op2 (nth expr 2 nil)]
      (match [oper op1 op2]
             ['not _ _] (str "(" "not " (parse op1) ")")
             ['and _ _] (str "(" (parse op1) " and " (parse op2) ")")
             ['or _ _] (str "(" (parse op1) " or " (parse op2) ")")))))

;; Fitness function

(defn error
  [individual]
  (let [formatted-indiv (str "\"" (parse individual) "\"")]
    (doto workspace
      (.command "setup")
      (.command (str "set gene " formatted-indiv))
      (.command (str "repeat " ticks " [ main ]"))))
  (- (* surge-income ticks) (.report workspace "mean [cash] of turtles")))

;; To help write mutation and crossover functions we'll write a utility
;; function that injects something into an expression and another that
;; extracts something from an expression.

(defn codesize
  [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn inject
  "Returns a copy of individual i with new inserted randomly somwhere
 within it (replacing something else)." 
  [new i]
  (if (and (not (terminal? i)) (seq? i))
    (if (zero? (rand-int (count (flatten i))))
      new
      (if (= (nth i 0) 'not) 
        (list (nth i 0) (inject new (nth i 1)))
        (if (< (rand)
               (/ (codesize (nth i 1))
                  (- (codesize i) 1)))
          (list (nth i 0) (inject new (nth i 1)) (nth i 2))
          (list (nth i 0) (nth i 1) (inject new (nth i 2))))))
    new))

(defn extract
  "Returns a random subexpression of individual i."
  [i]
  (if (and (not (terminal? i)) (seq? i))
    (if (zero? (rand-int (count (flatten i))))
      i
      (if (or (nil? (get i 2)) ;; FIX THIS
              (< (rand) (/ (codesize (nth i 1))
                           (- (codesize i)) 1)))
        (extract (nth i 1))
        (extract (nth i 2))))
    i))

;; Now the mutate and crossover functions are easy to write:

(defn mutate
  [i]
  (inject (random-code 2) i))

(defn crossover
  [i j]
  (inject (extract j) i))

;; We'll also want a way to sort a populaty by error that doesn't require
;; lots of error re-computation:

(defn sort-by-error
  [population]
  (map #(->gene (nth % 0) (nth % 1))
       (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population))))

;; Finally, we'll define a function to select an individual from a sorted
;; population using tournaments of a given size.

(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

;; Now we can evolve a solution by starting with a random population and
;; repeatedly sorting, checking for a solution, and producing a new
;; population.

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 2)))]
    (let [best (first population)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" (:error best))
      (println "Best program:" (:expr best))
      (println "     Median error:" (:error (nth population
                                                 (int (/ popsize 2)))))
      (println "     Average program size:"
               (float (/ (reduce + (map count (map flatten (map :expr population))))
                         (count population))))
      (if (< (:error best) 100) ;; good enough to count as success
        (println "Success:" best)
        (recur
         (inc generation)
         (sort-by-error
          (concat
           (repeatedly (* 1/2 popsize) #(mutate (select (map :expr population) 7)))
           (repeatedly (* 1/4 popsize) #(crossover (select (map :expr population) 7)
                                                   (select (map :expr population) 7)))
           (repeatedly (* 1/4 popsize) #(select (map :expr population) 7)))))))))

(defn -main
  []
  (use 'uber.core)
  (evolve 100)
  (.dispose workspace))

  
