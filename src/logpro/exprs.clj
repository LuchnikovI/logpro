(ns logpro.exprs
  (:require
   [clojure.string :refer [last-index-of]]
   [logpro.state :refer [get-mangling-counter! get-variable-id!]]
   [logpro.validation :refer [variable? validation-bug]]))

(defn third [expr] (nth expr 2))

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

(def get-varargs second)

(defn headed-form? [head expr]
  (and (compound-expr? expr) (= (first expr) head)))

(defn assertion? [expr] (headed-form? 'assert! expr))

(def get-assertion-body second)

(defn rule? [expr] (headed-form? 'rule expr))

(defn attach-postfix [variable sep postfix]
  (symbol (str variable sep postfix)))

(defn mangle-variable [variable postfix]
  (attach-postfix variable "@" postfix))

(defn unmangle-variable [variable]
  (let [variable (str variable)]
    (if-let [idx (last-index-of variable "@")]
      (symbol (subs variable 0 idx))
      variable)))

(defn attach-label-id! [var label-inst]
  (let [label-id (get-variable-id! label-inst)]
    (-> var
        unmangle-variable
        (attach-postfix "-" label-id))))

(defn mangle-rule [rule]
  (let [postfix (get-mangling-counter!)]
    (letfn [(helper [rule]
             (cond
               (variable? rule) (mangle-variable rule postfix)
               (= '() rule) '()
               (compound-expr? rule) (cons (helper (first rule)) (helper (rest rule)))
               :else rule))]
      (helper rule))))

(defn get-rule-body [rule]
  (case (count rule)
    2 nil
    3 (nth rule 2)
    (validation-bug)))

(def get-conclusion second)

(defn get-index [expr]
  (if (compound-expr? expr)
    (let [head (get-expr-head expr)]
      (if (and (symbol? head) (not (variable? head)))
        head
        nil))
    nil))

(defn and-query? [query] (headed-form? 'and query))

(defn get-and-body [and-query]
  (rest and-query))

(defn or-query? [query] (headed-form? 'or query))

(defn get-or-body [or-query]
  (rest or-query))

(defn is-query? [query] (headed-form? 'is query))

(def get-is-lhs second)

(def get-is-rhs third)

(defn not-query? [query] (headed-form? 'not query))

(def get-not-body second)

(defn clojure-pred-query? [query] (headed-form? 'clojure-predicate query))

(def get-clojure-pred-body rest)

(defn eq-query? [query] (headed-form? '= query))

(def get-eq-lhs second)

(def get-eq-rhs third)

(defn in-query? [query] (headed-form? 'in query))

(def get-in-lhs second)

(def get-in-rhs third)

(defn collect-query? [query] (headed-form? 'collect query))

(defn desug-collect-query [collect-query]
  (let [form-size (count collect-query)]
    (case form-size
      5 collect-query
      4 (list
         (nth collect-query 0)
         (nth collect-query 1)
         (nth collect-query 2)
         (nth collect-query 2)
         (nth collect-query 3))
      3 (list
         (nth collect-query 0)
         (nth collect-query 1)
         (nth collect-query 1)
         (nth collect-query 1)
         (nth collect-query 2))
      (validation-bug))))

(defn get-template [collect-query]
  (-> collect-query
      desug-collect-query
      (nth 1)))

(defn get-label [collect-query]
  (-> collect-query
      desug-collect-query
      (nth 2)))

(defn get-query [collect-query]
  (-> collect-query
      desug-collect-query
      (nth 3)))

(defn get-bag [collect-query]
  (-> collect-query
      desug-collect-query
      (nth 4)))

(defn range-query? [query] (headed-form? 'range query))

(def get-var second)

(defn get-start [range-query]
  (nth range-query 2 0))

(defn get-end [range-query]
  (nth range-query 3 Long/MAX_VALUE))

(defn get-step [rnage-query]
  (nth rnage-query 4 1))

(defn unify-query? [query] (headed-form? 'eq query))

(def get-unify-lhs second)

(def get-unify-rhs third)

(def numerical-ops #{'+ '- '/ '*})

(def numerical-literal? number?)

(defn numerical-op? [expr]
  (and (compound-expr? expr)
       (not (empty-expr? expr))
       (numerical-ops (get-expr-head expr))))

(defn make-subtract-expression [lhs rhs]
  (list '- lhs rhs))
