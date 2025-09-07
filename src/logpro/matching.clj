(ns logpro.matching
  (:require
   [logpro.exprs :refer [empty-expr? vararg-head? get-varargs
                         get-expr-head get-expr-tail variable?
                         compound-expr?]]
   [logpro.frames :refer [get-binding insert-binding invalid-frame
                          invalid-frame?]]))

(declare match)

(defn extend-if-consistent [pat dat frame]
  (let [val (get-binding frame pat)]
    (if val
      (match val dat frame)
      (insert-binding frame pat dat))))

(defn match-compounds [pat dat frame]
  (cond
    (and (empty-expr? pat) (empty-expr? dat)) frame
    (and (vararg-head? pat) (vararg-head? dat)) (match (get-varargs pat) (get-varargs dat) frame)
    (vararg-head? pat) (match (get-varargs pat) dat frame)
    (vararg-head? dat) (match pat (get-varargs dat) frame)
    (or (empty-expr? pat) (empty-expr? dat)) invalid-frame
    :else (->> frame
               (match (get-expr-head pat) (get-expr-head dat))
               (match (get-expr-tail pat) (get-expr-tail dat)))))

(defn match [pat dat frame]
  (cond
    (invalid-frame? frame) frame
    (variable? pat) (extend-if-consistent pat dat frame)
    (and (compound-expr? pat) (compound-expr? dat)) (match-compounds pat dat frame)
    (= pat dat) frame
    :else invalid-frame))
