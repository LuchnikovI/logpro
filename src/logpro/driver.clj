(ns logpro.driver
  (:require [clojure.edn :as edn]
            [logpro.exprs :refer
             [variable? compound-expr? empty-expr? get-expr-head get-expr-tail
              vararg-head? get-varargs is-query? and-query? or-query? rule? get-and-body
              get-or-body get-is-lhs get-is-rhs get-rule-index mangle-rule get-rule-body
              get-conclusion not-query? get-not-body clojure-pred-query? get-clojure-pred-body
              assertion? get-assertion-body unmangle-variable]]
            [logpro.frames :refer
             [get-binding insert-binding invalid-frame? invalid-frame
              filter-invalid-frames get-single-elem-stream empty-frames-stream
              flatmap instantiate instantiate-stream init-frames-stream]]))

;; data-base abstraction

(defn get-pat-index [pat]
  (if (compound-expr? pat)
    (let [head (get-expr-head pat)]
      (if (and (symbol? head) (not (variable? head)))
        head
        nil))
    nil))

(defn make-db []
  {:all-assertions []
   :indexed-assertions {}
   :all-rules []
   :indexed-rules {}})

(defn fetch-assertions [db pat]
  (if-let [idx (get-pat-index pat)]
    ((:indexed-assertions db) idx)
    (:all-assertions db)))

(defn fetch-rules [db pat]
  (if-let [idx (get-pat-index pat)]
    ((:indexed-rules db) idx)
    (:all-rules db)))

(defn insert-or-conj [m idx elem]
  (if (contains? m idx)
    (update m idx #(conj % elem))
    (assoc m idx [elem])))

(defn add-to-all-assertions [db assertion]
  (update db :all-assertions #(conj % assertion)))

(defn add-to-indexed-assertions [db idx assertion]
  (update
   db
   :indexed-assertions
   #(insert-or-conj % idx assertion)))

(defn add-assertion [db assertion]
  (add-to-all-assertions
   (if-let [idx (get-pat-index assertion)]
     (add-to-indexed-assertions db idx assertion)
     db)
   assertion))

(defn add-to-all-rules [db assertion]
  (update db :all-rules #(conj % assertion)))

(defn add-to-indexed-rules [db idx assertion]
  (update
   db
   :indexed-rules
   #(insert-or-conj % idx assertion)))

(defn add-rule [db rule]
  (add-to-all-rules
   (if-let [idx (get-rule-index rule)]
     (add-to-indexed-rules db idx rule)
     db)
   rule))

(defn init-db [db [fst & rst]]
  (if (nil? fst)
    db
    (init-db
     (if (rule? fst)
       (add-rule db fst)
       (add-assertion db fst))
     rst)))

;; pattern matching

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

;; unification

(defn depends-on? [val var frame]
  (cond
    (variable? val) (if (= val var)
                      true
                      (let [val-lhs (get-binding frame val)]
                        (if val-lhs
                          (depends-on? val-lhs var frame)
                          false)))
    (compound-expr? val) (if (empty-expr? val)
                           false
                           (or
                            (depends-on? (get-expr-head val) var frame)
                            (depends-on? (get-expr-tail val) var frame)))
    :else false))

(declare unify)

(defn extend-if-possible [var val frame]
  (let [val-lhs (get-binding frame var)]
    (cond
      val-lhs (unify val-lhs val frame)
      (variable? val) (let [val-rhs (get-binding frame val)]
                        (if val-rhs
                          (unify var val-rhs frame)
                          (insert-binding frame var val)))
      (depends-on? val var frame) invalid-frame
      :else (insert-binding frame var val))))

(defn unify-compounds [lhs-dat rhs-dat frame]
  (cond
    (and (empty-expr? lhs-dat) (empty-expr? rhs-dat)) frame
    (and (vararg-head? lhs-dat) (vararg-head? rhs-dat)) (unify
                                                         (get-varargs lhs-dat)
                                                         (get-varargs rhs-dat)
                                                         frame)
    (vararg-head? lhs-dat) (unify (get-varargs lhs-dat) rhs-dat frame)
    (vararg-head? rhs-dat) (unify lhs-dat (get-varargs rhs-dat) frame)
    (or (empty-expr? lhs-dat) (empty-expr? rhs-dat)) invalid-frame
    :else (->> frame
               (unify (get-expr-head lhs-dat) (get-expr-head rhs-dat))
               (unify (get-expr-tail lhs-dat) (get-expr-tail rhs-dat)))))

(defn unify [p1 p2 frame]
  (cond
    (invalid-frame? frame) frame
    (variable? p1) (extend-if-possible p1 p2 frame)
    (variable? p2) (extend-if-possible p2 p1 frame)
    (and (compound-expr? p1) (compound-expr? p2)) (unify-compounds p1 p2 frame)
    (= p1 p2) frame
    :else invalid-frame))

;; eval primitives

(declare ev-query)

(defn find-assertions [query db frame]
  (filter-invalid-frames
   (map
    #(match query % frame)
    (fetch-assertions db query))))

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

(defn ev-is-query [is-query _ frames]
  (let [lhs (get-is-lhs is-query)
        rhs (get-is-rhs is-query)]
    (keep (fn [frame]
            (let [rhs-eval (eval
                            (instantiate
                             rhs
                             frame
                             (fn [var _]
                               (throw
                                (ex-info
                                 "RHS of IS query must be fully instantiated!"
                                 {'unbound-variable var})))))]
              (if (variable? lhs)
                (unify lhs rhs-eval frame)
                (throw (ex-info "LHS of IS query must be a variable" {'lhs lhs})))))
          frames)))

(defn apply-clojure-func [func queries frame]
  (let [args (into [] (map
                        (fn [query]
                          (instantiate
                           query
                           frame
                           (fn [expr _]
                             (throw (ex-info "Unknown variable!" {'unknown-variable expr})))))
                        queries))]
    (try
      (apply
       func
       args)
      (catch Exception e (throw (ex-info "Clojure exception while applying Clojure predicate!", {'clojure-exception-message (ex-message e) 'arguments args}))))))

(defn ev-clojure-pred-query [clojure-pred-query _ frames]
  (let [[func-str & queries] (get-clojure-pred-body clojure-pred-query)
        func (try (eval (read-string func-str)) (catch Exception e (throw (ex-info "Cannot evaluate Clojure predictae!", {'clojure-predicate (read-string func-str), 'clojure-exception-message (ex-message e)}))))]
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

;; prompts

(def assertion-added-prompt "Assertion added")

(def rule-added-prompt "Rule added")

(def input-prompt ">> Query input:")

(def end-prompt ">> End")

(defn emit-prompt [prompt] (println prompt))

(defn read-input [] (edn/read-string (read-line)))

(defmulti display-result
  (fn [response]
    (:type response)))

(defmethod display-result :assertion-added [_]
  (println)
  (emit-prompt assertion-added-prompt)
  (println))

(defmethod display-result :rule-added [_]
  (println)
  (emit-prompt rule-added-prompt)
  (println))

(defmethod display-result :error [error]
  (println)
  (emit-prompt (:results error))
  (println))

(defmethod display-result :query [response]
  (println)
  (letfn [(helper [response]
            (if-let [current-result (first response)]
              (do (emit-prompt current-result)
                (let [input (read-input)]
                  (if (= input 'stop)
                    (do (emit-prompt end-prompt)
                        nil)
                    (helper (rest response)))))
              (do (emit-prompt end-prompt)
                  (println)
                  nil)))]
    (helper (:results response))))

;; core eval and driver loop

(defn ev [expr db frames]
    (cond
      (assertion? expr) (let [assertion-body (get-assertion-body expr)]
                          (if (rule? assertion-body)
                            {:type :rule-added
                             :db (add-rule db expr)
                             :results nil}
                            {:type :assertion-added
                             :db (add-assertion db expr)
                             :results nil}))
      :else (let [frames (ev-query expr db frames)
                  results (instantiate-stream expr frames (fn [expr _] (unmangle-variable expr)))]
              {:type :query
               :db db
               :results results})))

(defn get-error-result [exception db]
  {:type :error
   :db db
   :results (format "Query was executed with error: %s, data: %s" (ex-message exception) (ex-data exception))})

(defn run-driver-loop [db frames]
  (emit-prompt input-prompt)
  (let [response (try (let [response (ev (read-input) db frames)]
                         (display-result response)
                         response)
                      (catch Exception e (let [response (get-error-result e db)]
                                           (display-result response)
                                           response)))]
    (recur (:db response) (init-frames-stream))))

