(ns lambda-calculus.strategy.strategy)

(defprotocol Strategy
  (iter [strategy term]))