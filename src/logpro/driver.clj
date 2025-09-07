(ns logpro.driver
  (:require [clojure.edn :as edn]
            [clojure.string :refer [last-index-of]]
            [logpro.exprs :refer [variable?]]
            [logpro.frames :refer
             [get-binding insert-binding invalid-frame? invalid-frame
              filter-invalid-frames get-single-elem-stream empty-frames-stream
              flatmap instantiate instantiate-stream init-frames-stream]]))

;; mangling counter

(def mangling-counter (atom 0))

(defn get-mangling-counter! []
  (let [curr-counter @mangling-counter]
    (swap! mangling-counter inc)
    curr-counter))

;; assertion abstraction

(defn assertion? [expr]
  (and (sequential? expr) (= (first expr) 'assert!)))

(defn get-assertion-body [assertion]
  (if (not= (count assertion) 2)
    (throw (ex-info "Bad assertion!", {'assertion assertion}))
    (nth assertion 1)))

;; rule abstraction

(defn rule? [rule]
  (and (sequential? rule) (= (first rule) 'rule)))

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
  (nth rule 1))

(defn get-rule-index [rule]
  (let [fst (first (get-conclusion rule))]
    (if (and (not (variable? fst)) (symbol? fst))
      fst
      nil)))

;; data-base abstraction

(defn get-pat-index [pat]
  (if (sequential? pat)
    (let [fst (first pat)]
      (if (and (symbol? fst) (not (variable? fst)))
        fst
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

(defn match-seqs [pat dat frame]
  (cond
    (and (empty? pat) (empty? dat)) frame
    (and (= (first pat) '.) (= (first dat) '.)) (match (nth pat 1) (nth dat 1) frame)
    (= (first pat) '.) (match (nth pat 1) dat frame)
    (= (first dat) '.) (match pat (nth dat 1) frame)
    (or (empty? pat) (empty? dat)) invalid-frame
    :else (match (rest pat) (rest dat) (match (first pat) (first dat) frame))))

(defn match [pat dat frame]
  (cond
    (invalid-frame? frame) frame
    (variable? pat) (extend-if-consistent pat dat frame)
    (and (sequential? pat) (sequential? dat)) (match-seqs pat dat frame)
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
    (sequential? val) (if (empty? val)
                        false
                        (or (depends-on? (first val) var frame) (depends-on? (rest val) var frame)))
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

(defn unify-seqs [lhs-dat rhs-dat frame]
  (cond
    (and (empty? lhs-dat) (empty? rhs-dat)) frame
    (and (= (first lhs-dat) '.) (= (first rhs-dat) '.)) (unify (nth lhs-dat 1) (nth rhs-dat 1) frame)
    (= (first lhs-dat) '.) (unify (nth lhs-dat 1) rhs-dat frame)
    (= (first rhs-dat) '.) (unify lhs-dat (nth rhs-dat 1) frame)
    (or (empty? lhs-dat) (empty? rhs-dat)) invalid-frame
    :else (unify (rest lhs-dat) (rest rhs-dat) (unify (first lhs-dat) (first rhs-dat) frame))))

(defn unify [p1 p2 frame]
  (cond
    (invalid-frame? frame) frame
    (variable? p1) (extend-if-possible p1 p2 frame)
    (variable? p2) (extend-if-possible p2 p1 frame)
    (and (sequential? p1) (sequential? p2)) (unify-seqs p1 p2 frame)
    (= p1 p2) frame
    :else invalid-frame))

;; compound queries

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

