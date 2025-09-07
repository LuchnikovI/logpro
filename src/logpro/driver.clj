(ns logpro.driver
  (:require [clojure.edn :as edn]
            [logpro.exprs :refer
             [variable? is-query? and-query? or-query? rule? get-and-body
              get-or-body get-is-lhs get-is-rhs mangle-rule get-rule-body
              get-conclusion not-query? get-not-body clojure-pred-query?
              get-clojure-pred-body assertion? get-assertion-body unmangle-variable]]
            [logpro.frames :refer
             [invalid-frame? filter-invalid-frames get-single-elem-stream
              empty-frames-stream flatmap instantiate instantiate-stream
              init-frames-stream]]
            [logpro.db :refer [fetch-rules fetch-assertions add-rule add-assertion]]
            [logpro.matching :refer [match]]
            [logpro.unification :refer [unify]]))

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

