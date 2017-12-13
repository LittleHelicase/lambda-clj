(ns lambda-calculus.core-test
  (:require [clojure.test :refer :all]
            [lambda-calculus.core :as c]
            [lambda-calculus.rewrite.rule :as r]))

; (deftest stringify-lambdas
;   (testing "Converts the identity function into the string 'λx. x'."
;     (is (= "λx. x" (to-string '(fn [x] x)))))
;   (testing "Converts self application to string 'λx. x x'"
;     (is (=  "λx. (x x)" (to-string '(fn [x] (x x))))))
;   (testing "Handles functions in functions."
;     (is (= "λx. (λy. (x y))" (to-string '(fn [x] (fn [y] (x y)))))))
;   (testing "Converts a simple expression"
;     (is (= "x" (to-string 'x))))
;   (testing "Converts a simple function application"
;     (is (= "x y" (to-string '(x y)))))
;   (testing "Handles functions in functions."
;     (is (= "((λx. (λy. (x y))) (λz. z)) 6" (to-string '(((fn [x] (fn [y] (x y))) (fn [z] z)) 6)))))
;   (testing "Adds parenthesis if necessary."
;     (is (= "(λx. x) 6" (to-string '((fn [x] x) 6)))))
;   (testing "Can handle # lambdas."
;     (is (= "(λx. (x x)) 6" (to-string '(#(%1 %1) 6))))))

(deftest equality
  (testing "Compares by AST"
    (is (= '(fn [x] x) '(fn [x] x)))
    (is (= '(fn [y] y) '(fn [y] y)))
    (is (= '(fn [x y] (x y)) '(fn [x  y] (x  y))))
    (is (not= '(fn [x y] (y x)) '(fn [x y] (x y))))
    (is (not= '(fn [x] x) '(fn [y] y)))))

(deftest rewriting-rules
  (testing "Template replacements"
    (let [trr (r/template-replace-rule 'arg '[x])]
      (is (r/match trr 'arg))
      (is (= '[x] (r/apply trr 'arg)))))
  (testing "Variable matching"
    (is (= (r/subs '(fn [%1] %1) '(fn [x] x)) {:%1 'x}))))