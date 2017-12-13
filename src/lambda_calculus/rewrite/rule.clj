(ns lambda-calculus.rewrite.rule
  (:require [clojure.zip :as z]))

(defn- quotation-zipper-s [q] (z/zipper seq? seq (fn [_ c] c) q))
(defn- pair? [x] (and (or (seq? x) (vector? x)) (= (count x) 2)))

(defprotocol Rule
  (match [rule node])
  (apply [rule node]))

(defrecord TemplateReplaceRule [template replacement]
  Rule
  (match [rule node] (= template node))
  (apply [rule node] replacement))


(defn allPattern [pattern]
  (let [q (quotation-zipper-s (macroexpand pattern))]
    (loop [q q
           w []]
      (if (z/end? q)
          w
          (recur (z/next q) (if (not (seq? (z/node q))) (conj w (z/node q)) w))))))

(defn subs-p [p1 p2 f]
  (let [res {}]
    (if (not= (count p1) (count p2))
      {}
      (reduce f {} (map vector p1 p2)))))

(def wildcards #{:%1 :%2 :%3 :%4 :%5 :%6 :%7 :%8 :%9})

(defn assignments [i]
  (if (not (pair? i))
    {}
    (if (contains? wildcards (keyword (first i)))
      (assoc {} (keyword (first i)) (second i))
      {})))

(defn subs [p1 p2]
  (subs-p p1 p2 (fn [res i] (merge res (assignments i)))))

(defn template-replace-rule [template replacement]
  (TemplateReplaceRule. template replacement))
