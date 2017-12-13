(ns lambda-calculus.strategy.string
  (:require [lambda-calculus.strategy.strategy :as strategy])
  (:require [clojure.string :as s])
  (:require [clojure.zip :as z]))

(defn- quotation-zipper [q] (z/zipper list? seq (fn [_ c] c) q))

(def ^{:private true} fnType (first '(fn)))
(def ^{:private true} fnStarType (first '(fn*)))

(defn- stringify
  "Convert a quoted expression into a string. Creating parenthesis where necessary etc."
  [quoted]
  (cond
    (= fnType quoted) "λ"
    (= fnStarType quoted) "λ"
    (vector? quoted) (str (s/join " " quoted) ".")
    (list? quoted) ""
    :else (str quoted)))

(defrecord StringStrategy []
  strategy/Strategy
  (iter [strategy term]
    (let [q (z/next (quotation-zipper term))]
      (s/trim (s/replace
        (loop [q q
              cur ""]
          (if (z/end? q)
            cur
            (recur (z/next q) (str cur " " (stringify (z/node q))))))
        "λ " "λ")))))

(defn new-string-strategy []
  (StringStrategy.))
