(ns logpro.ev
  (:require [logpro.frames :refer [filter-invalid-frames flatmap invalid-frame? empty-frames-stream
                                   get-single-elem-stream instantiate instantiate-stream]]
            [logpro.matching :refer [match]]
            [logpro.db :refer [fetch-assertions fetch-rules add-assertion add-rule]]
            [logpro.exprs :refer [mangle-rule get-conclusion get-rule-body get-and-body get-or-body
                                  get-not-body get-is-lhs get-is-rhs get-clojure-pred-body
                                  clojure-pred-query? not-query? is-query? and-query? or-query?
                                  assertion? rule? get-assertion-body unmangle-variable eq-query?
                                  get-eq-lhs get-eq-rhs range-query? get-var get-start get-step
                                  get-end in-query? get-in-lhs get-in-rhs get-label-id! get-query
                                  get-label get-bag get-template collect-query? attach-postfix
                                  get-unify-lhs get-unify-rhs unify-query?]]
            [logpro.unification :refer [unify unify-with-every]]
            [logpro.constraints-engine :refer [add-equality-constraint]]))

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

(defn handle-unbounds-in-is-rhs [var _]
  (throw (ex-info "RHS of IS query must be fully instantiated!" {'unbound-variable var})))

(defn instantiate-and-eval-is-rhs [expr frame]
  (eval (instantiate expr frame handle-unbounds-in-is-rhs)))

(defn ev-is-query [is-query _ frames]
  (let [lhs (get-is-lhs is-query)
        rhs (get-is-rhs is-query)]
    (keep
     (fn [frame]
       (let [rhs-eval (instantiate-and-eval-is-rhs rhs frame)]
         (unify lhs rhs-eval frame)))
     frames)))

(defn instantiate-arithmetic-expr [expr frame]
  (instantiate expr frame (fn [var _] var)))

(defn ev-eq-query [eq-query _ frames]
  (let [lhs (get-eq-lhs eq-query)
        rhs (get-eq-rhs eq-query)]
    (keep
     #(add-equality-constraint
       %
       (instantiate-arithmetic-expr lhs %)
       (instantiate-arithmetic-expr rhs %))
     frames)))

(defn get-values-range [start end step]
  (case end
    inf (map #(+ start (* step %)) (range))
    (range start end step)))

(defn match-var-with-values [var values frame]
  (map #(match var % frame) values))

(defn ev-range-query [range-query _ frames]
  (let [var (get-var range-query)
        start (get-start range-query)
        end (get-end range-query)
        step (get-step range-query)
        values (get-values-range start end step)]
    (flatmap
     #(match-var-with-values var values %)
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

(defn ev-in-query [in-query _ frames]
  (let [lhs (get-in-lhs in-query)
        rhss (get-in-rhs in-query)]
    (filter (comp not invalid-frame?)
     (flatmap
      #(unify-with-every lhs rhss %)
      frames))))

(defn attach-label-id! [var label-inst]
  (let [label-id (get-label-id! label-inst)]
    (-> var
        unmangle-variable
        (attach-postfix "-" label-id))))

(defn instantiate-label [label frame]
  (instantiate label frame (fn [expr _] (unmangle-variable expr))))

(defn instantiate-template [template frame label]
  (let [inst-label (instantiate-label label frame)]
    (instantiate template frame (fn [var _] (attach-label-id! var inst-label)))))

(defn ev-to-list [template label query bag db frame]
  (as-> (ev-query query db [frame]) $
    (map #(instantiate-template template % label) $)
    (unify bag $ frame)))

(defn ev-collect-query [collect-query db frames]
  (let [template (get-template collect-query)
        query (get-query collect-query)
        label (get-label collect-query)
        bag (get-bag collect-query)]
    (keep
     #(ev-to-list template label query bag db %)
     frames)))

(defn ev-unify-query [query _ frames]
  (let [lhs (get-unify-lhs query)
        rhs (get-unify-rhs query)]
    (map #(unify lhs rhs %) frames)))

(defn ev-query [query db frames]
  (cond
    (unify-query? query) (ev-unify-query query db frames)
    (and-query? query) (ev-and-query query db frames)
    (or-query? query) (ev-or-query query db frames)
    (not-query? query) (ev-not-query query db frames)
    (is-query? query) (ev-is-query query db frames)
    (clojure-pred-query? query) (ev-clojure-pred-query query db frames)
    (eq-query? query) (ev-eq-query query db frames)
    (range-query? query) (ev-range-query query db frames)
    (in-query? query) (ev-in-query query db frames)
    (collect-query? query) (ev-collect-query query db frames)
    :else (ev-simple-query query db frames)))

(defn ev [expr db frames]
  (cond
    (nil? expr) {:type :end-of-file}
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

