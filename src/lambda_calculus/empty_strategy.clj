(ns lambda-calculus.strategy)

(deftype empty-strategy []
  Strategy
  (iter [strategy term]
    term))