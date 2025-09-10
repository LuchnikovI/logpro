(ns logpro.ev
  (:require [logpro.frames :refer [filter-invalid-frames flatmap invalid-frame? empty-frames-stream
                                   get-single-elem-stream instantiate instantiate-stream]]
            [logpro.matching :refer [match]]
            [logpro.db :refer [fetch-assertions fetch-rules add-assertion add-rule]]
            [logpro.exprs :refer [mangle-rule get-conclusion get-rule-body get-and-body get-or-body
                                  get-not-body get-is-lhs get-is-rhs variable? get-clojure-pred-body
                                  clojure-pred-query? not-query? is-query? and-query? or-query?
                                  assertion? rule? get-assertion-body unmangle-variable]]
            [logpro.unification :refer [unify]]))

(defn find-assertions [query db frame]
  (filter-invalid-frames
   (map
    #(match query % frame)
    (fetch-assertions db query))))

(declare ev-query)

(defn apply-rules  [query db frame]
  (filter-invalid-frames
   (flatmap
    (fn [rule]
      (let [mangled-rule (mangle-rule rule)
            new-frame (unify query (get-conclusion mangled-rule) frame)]
        (if (invalid-frame? new-frame)
          empty-frames-stream
          (let [pat (get-rule-body mangled-rule)]
            (if (nil? pat)
              (get-single-elem-stream new-frame)
              (ev-query pat db (get-single-elem-stream new-frame)))))))
    (fetch-rules db query))))

(defn ev-simple-query [query db frames]
  (flatmap
   (fn [frame]
     (concat
      (find-assertions query db frame)
      (apply-rules query db frame)))
   frames))

(defn ev-and-query [and-query db frames]
  (let [queries (get-and-body and-query)]
    (letfn [(helper [queries frames]
              (if (empty? queries)
                frames
                (recur (rest queries) (ev-query (first queries) db frames))))]
      (helper queries frames))))

(defn ev-or-query [or-query db frames]
  (flatmap #(ev-query % db frames) (get-or-body or-query)))

(defn ev-not-query [not-query db frames]
  (let [query (get-not-body not-query)]
    (filter
     #(let [matches (ev-query query db (get-single-elem-stream %))]
        (empty? matches))
     frames)))

(defn handle-unbounds-in-arithmetic-expr [var _]
  (throw (ex-info "RHS of IS query must be fully instantiated!" {'unbound-variable var})))

(defn instantiate-arithmetic-expr [expr frame]
  (eval (instantiate expr frame handle-unbounds-in-arithmetic-expr)))

(defn ev-is-query [is-query _ frames]
  (let [lhs (get-is-lhs is-query)
        rhs (get-is-rhs is-query)]
    (keep
     (fn [frame]
       (let [rhs-eval (instantiate-arithmetic-expr rhs frame)]
         (if (variable? lhs)
           (unify lhs rhs-eval frame)
           (throw (ex-info "LHS of IS query must be a variable" {'lhs lhs})))))
     frames)))

(defn handle-unknown-var [var _]
  (throw (ex-info "Unknown variable!" {'unknown-variable var})))

(defn instantiate-args [args frame]
  (into [] (map #(instantiate % frame handle-unknown-var) args)))

(defn apply-clojure-func [func args frame]
  (let [inst-args (instantiate-args args frame)]
    (try
      (apply func inst-args)
      (catch Exception e
        (throw
         (ex-info
          "Clojure exception while applying Clojure predicate!"
          {'clojure-exception-message (ex-message e)
           'arguments args}))))))

(defn read-eval-clojure-func [func-str]
  (try
    (eval (read-string func-str))
    (catch Exception e
      (throw
       (ex-info
        "Cannot evaluate Clojure predictae!"
        {'clojure-predicate (read-string func-str)
         'clojure-exception-message (ex-message e)})))))

(defn ev-clojure-pred-query [clojure-pred-query _ frames]
  (let [[func-str & queries] (get-clojure-pred-body clojure-pred-query)
        func (read-eval-clojure-func func-str)]
    (filter
     #(apply-clojure-func func queries %)
     frames)))

(defn ev-query [query db frames]
  (cond
    (and-query? query) (ev-and-query query db frames)
    (or-query? query) (ev-or-query query db frames)
    (not-query? query) (ev-not-query query db frames)
    (is-query? query) (ev-is-query query db frames)
    (clojure-pred-query? query) (ev-clojure-pred-query query db frames)
    :else (ev-simple-query query db frames)))

(defn ev-can-throw [expr db frames]
    (cond
      (assertion? expr) (let [assertion-body (get-assertion-body expr)]
                          (if (rule? assertion-body)
                            {:type :rule-added
                             :db (add-rule db expr)}
                            {:type :assertion-added
                             :db (add-assertion db expr)}))
      :else (let [frames (ev-query expr db frames)
                  results (instantiate-stream expr frames (fn [expr _] (unmangle-variable expr)))]
              {:type :query
               :db db
               :results results})))

(defn get-error-result [exception db]
  {:type :error
   :db db
   :error (format "Prompt was executed with error: %s, data: %s" (ex-message exception) (ex-data exception))})

(defn ev [delayed-expr db frames]
  (try (ev-can-throw @delayed-expr db frames)
       (catch Exception e
         (get-error-result e db))))

