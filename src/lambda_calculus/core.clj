(ns lambda-calculus.core
  (require [lambda-calculus.strategy.strategy :refer :all])
  (require [lambda-calculus.strategy.string :refer :all]))
(use '[clojure.string])

(def fnType (first '(fn)))
(def fnStarType (first '(fn*)))

(defn- embrace [input] (str "(" input ")"))

(defn- stringify
  "Convert a quoted expression into a string. Creating parenthesis where necessary etc."
  [is-child quoted]
  (cond
    (= fnType quoted) "λ"
    (= fnStarType quoted) "λ"
    (vector? quoted) (str (clojure.string/join " " quoted) ".")
    (and is-child (list? quoted)) (embrace (trim (clojure.string/join " " (map (partial stringify true) quoted))))
    (list? quoted) (trim (clojure.string/join " " (map (partial stringify true) quoted)))
    :else (str quoted)))

(defn reduce-term
  "Evaluate a lambda expression using a reduction strategy"
  [term strategy]
  (iter strategy term))

; (defn to-string
;   "Convert a lambda expression into a string that represents the formula in lambda calculus."
;   [quoted]
;   (clojure.string/replace ; we want 'λx' and not 'λ x' so we get rid of the space in the last step
;     (stringify false quoted)
;     "λ " "λ"))
(defn to-string [q]
  (reduce-term q (new-string-strategy)))
