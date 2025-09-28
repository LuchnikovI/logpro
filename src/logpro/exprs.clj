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

(defn headed-form? [head expr]
  (and (compound-expr? expr) (= (first expr) head)))

(defn assertion? [expr] (headed-form? 'assert! expr))

(defn get-assertion-body [assertion]
  (if (not= (count assertion) 2)
    (throw (ex-info "Bad assertion!", {'assertion assertion}))
    (nth assertion 1)))

(defn rule? [expr] (headed-form? 'rule expr))

(def mangling-counter (atom 0))

(def label-ids (atom {}))

(defn get-label-id! [smthng]
  (if-let [label-id (@label-ids smthng)]
    label-id
    (let [new-label-id (count @label-ids)]
      (swap! label-ids assoc smthng new-label-id)
      new-label-id)))

(defn get-mangling-counter! []
  (let [curr-counter @mangling-counter]
    (swap! mangling-counter inc)
    curr-counter))

(defn attach-postfix [variable sep postfix]
  (symbol (str variable sep postfix)))

(defn mangle-variable [variable postfix]
  (attach-postfix variable "@" postfix))

(defn unmangle-variable [variable]
  (let [variable (str variable)]
    (if-let [idx (last-index-of variable "@")]
      (symbol (subs variable 0 idx))
      variable)))

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
    1 (throw (ex-info "Bad rule!", {'rule rule}))
    2 nil
    3 (nth rule 2)))

(defn get-conclusion [rule]
  (assert (>= (count rule) 2))
  (let [conclusion (nth rule 1)]
    (if (compound-expr? conclusion)
      conclusion
      (throw (ex-info "Conclusion of a rule must be a compound expression!" {'conclusion conclusion})))))

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

(defn get-is-lhs [is-query]
  (if (not= (count is-query) 3)
    (throw (ex-info "Bad IS query" {'is-query is-query}))
    (nth is-query 1)))

(defn get-is-rhs [is-query]
  (if (not= (count is-query) 3)
      (throw (ex-info "Bad IS query" {'is-query is-query}))
      (nth is-query 2)))

(defn not-query? [query] (headed-form? 'not query))

(defn get-not-body [query]
  (if (not= (count query) 2)
    (throw (ex-info "Bad NOT query!", {'not-query query}))
    (nth query 1)))

(defn clojure-pred-query? [query] (headed-form? 'clojure-predicate query))

(defn get-clojure-pred-body [clojure-pred-query]
  (if (< (count clojure-pred-query) 3)
    (throw (ex-info "Bad Clojure predicate query!", {'clojure-preidcate-query clojure-pred-query}))
    (rest clojure-pred-query)))

(defn eq-query? [query] (headed-form? 'eq query))

(defn get-eq-lhs [eq-query]
  (if (not= (count eq-query) 3)
    (throw (ex-info "Bad EQ query" {'eq-query eq-query}))
    (nth eq-query 1)))

(defn get-eq-rhs [eq-query]
  (if (not= (count eq-query) 3)
      (throw (ex-info "Bad IS query" {'eq-query eq-query}))
      (nth eq-query 2)))

(defn in-query? [query] (headed-form? 'in query))

(defn get-in-lhs [in-query]
  (if (not= (count in-query) 3)
      (throw (ex-info "Bad IN query" {'in-query in-query}))
      (nth in-query 1)))

(defn get-in-rhs [in-query]
  (if (not= (count in-query) 3)
      (throw (ex-info "Bad IN query" {'in-query in-query}))
      (nth in-query 2)))

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
      (throw (ex-info "Bad COLLECT query" {'collect-query collect-query})))))

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

;; TODO: type checking for rnage parameters

(defn range-query? [query] (headed-form? 'range query))

(defn get-var [range-query]
  (if (< (count range-query) 2) (throw (ex-info "Variables is not specified in rnage query!" {'range-query range-query}))
      (nth range-query 1)))

(defn get-start [range-query]
  (nth range-query 2 0))

(defn get-end [range-query]
  (nth range-query 3 'inf))

(defn get-step [rnage-query]
  (nth rnage-query 4 1))

(defn unify-query? [query] (headed-form? '= query))

(defn get-unify-lhs [unify-query]
  (if (not= (count unify-query) 3)
      (throw (ex-info "Bad unify query" {'unify-query unify-query}))
      (nth unify-query 1)))

(defn get-unify-rhs [unify-query]
  (if (not= (count unify-query) 3)
      (throw (ex-info "Bad unify query" {'unify-query unify-query}))
      (nth unify-query 2)))

(def numerical-ops #{'+ '- '/ '*})

(def numerical-literal? number?)

(defn numerical-op? [expr]
  (and (compound-expr? expr)
       (not (empty-expr? expr))
       (numerical-ops (get-expr-head expr))))

(defn make-subtract-expression [lhs rhs]
  (list '- lhs rhs))
