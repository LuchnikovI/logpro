(ns logpro.exprs
  (:require
   [clojure.string :refer [last-index-of]]))

(defn variable? [expr]
  (and (symbol? expr) (= (first (str expr)) \?)))

(def compound-expr? sequential?)

(def empty-expr? empty?)

(def empty-expr '())

(def get-expr-head first)

(def get-expr-tail rest)

(def vararg-symbol '.)

(defn vararg-head? [expr] (= (get-expr-head expr) vararg-symbol))

(defn make-pair-expression [lhs rhs] (list lhs rhs))

(defn extend-compound-expr [extension expr]
  (cons extension expr))

(defn get-varargs [expr]
  (assert (vararg-head? expr))
  (if (= (count expr) 2)
    (nth expr 1)
    (throw (ex-info
            "Vararg symbol must be folloed by a single expression!"
            {'vararg-symbol vararg-symbol
             'after-vararg-symbol (rest expr)}))))

(defn assertion? [expr]
  (and (sequential? expr) (= (first expr) 'assert!)))

(defn get-assertion-body [assertion]
  (if (not= (count assertion) 2)
    (throw (ex-info "Bad assertion!", {'assertion assertion}))
    (nth assertion 1)))

(defn rule? [rule]
  (and (sequential? rule) (= (first rule) 'rule)))

(def mangling-counter (atom 0))

(defn get-mangling-counter! []
  (let [curr-counter @mangling-counter]
    (swap! mangling-counter inc)
    curr-counter))

(defn mangle-variable [variable postfix]
  (symbol (str variable "-" postfix)))

(defn unmangle-variable [variable]
  (let [variable (str variable)
        idx (last-index-of variable "-")]
    (symbol (subs variable 0 idx))))

(defn mangle-rule [rule]
  (let [postfix (get-mangling-counter!)]
    (letfn [(helper [rule]
             (cond
               (variable? rule) (mangle-variable rule postfix)
               (= '() rule) '()
               (sequential? rule) (cons (helper (first rule)) (helper (rest rule)))
               :else rule))]
      (helper rule))))

(defn get-rule-body [rule]
  (case (count rule)
    1 (throw (ex-info "Bad rule!", {'rule rule}))
    2 nil
    3 (nth rule 2)))

(defn get-conclusion [rule]
  (assert (>= (count rule) 2))
  (nth rule 1))

(defn get-rule-index [rule]
  (let [head (first (get-conclusion rule))]
    (if (and (not (variable? head)) (symbol? head))
      head
      nil)))

(defn and-query? [query]
  (and (sequential? query) (= (first query) 'and)))

(defn get-and-body [and-query]
  (rest and-query))

(defn or-query? [query]
  (and (sequential? query) (= (first query) 'or)))

(defn get-or-body [or-query]
  (rest or-query))

(defn is-query? [is-query]
  (and (sequential? is-query) (= (first is-query) 'is)))

(defn get-is-lhs [is-query]
  (if (not= (count is-query) 3)
    (throw (ex-info "Bad IS query" {'is-query is-query}))
    (nth is-query 1)))

(defn get-is-rhs [is-query]
  (if (not= (count is-query) 3)
      (throw (ex-info "Bad IS query" {'is-query is-query}))
      (nth is-query 2)))

(defn not-query? [query]
  (and (sequential? query) (= (first query) 'not)))

(defn get-not-body [query]
  (if (not= (count query) 2)
    (throw (ex-info "Bad NOT query!", {'not-query query}))
    (nth query 1)))

(defn clojure-pred-query? [query]
  (and (sequential? query) (= (first query) 'clojure-predicate)))

(defn get-clojure-pred-body [clojure-pred-query]
  (if (< (count clojure-pred-query) 3)
    (throw (ex-info "Bad Clojure predicate query!", {'clojure-preidcate-query clojure-pred-query}))
    (rest clojure-pred-query)))
