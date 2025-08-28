(ns logpro.driver
  (:require [clojure.edn :as edn]
            [clojure.string :refer [last-index-of]]))

(defn variable? [pat]
  (and (symbol? pat) (= (first (str pat)) \?)))

;; mangling counter

(def mangling-counter (atom 0))

(defn get-mangling-counter! []
  (let [curr-counter @mangling-counter]
    (swap! mangling-counter inc)
    curr-counter))

;; stream helpers

(defmacro single-elem-stream [expr]
  `(lazy-seq (cons ~expr nil)))

(defn intrlv [iter-iter]
  (when (some seq iter-iter)
    (concat
     (keep first iter-iter)
     (intrlv (map rest iter-iter)))))

;; mapcat with interleaving
(defn flatmap [func iter]
  (lazy-seq (->> iter
                (map func)
                intrlv)))

(defn init-frames []
  (single-elem-stream {}))

;; assertion abstraction

(defn assertion? [expr]
  (and (sequential? expr) (= (first expr) 'assert!)))

(defn get-assertion-body [assertion]
  (nth assertion 1))

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
  (try
    (nth rule 2)
    (catch Exception _ nil)))

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
  (let [val (frame pat)]
    (if val
      (match val dat frame)
      (assoc frame pat dat))))

(defn match-seqs [pat dat frame]
  (cond
    (and (empty? pat) (empty? dat)) frame
    (and (= (first pat) '.) (= (first dat) '.)) (match (nth pat 1) (nth dat 1) frame)
    (= (first pat) '.) (match (nth pat 1) dat frame)
    (= (first dat) '.) (match pat (nth dat 1) frame)
    (or (empty? pat) (empty? dat)) nil
    :else (match (rest pat) (rest dat) (match (first pat) (first dat) frame))))

(defn match [pat dat frame]
  (cond
    (nil? frame) frame
    (variable? pat) (extend-if-consistent pat dat frame)
    (and (sequential? pat) (sequential? dat)) (match-seqs pat dat frame)
    (= pat dat) frame))

;; unification

(defn depends-on? [val var frame]
  (cond
    (variable? val) (if (= val var)
                      true
                      (let [val-lhs (frame val)]
                        (if val-lhs
                          (depends-on? val-lhs var frame)
                          false)))
    (sequential? val) (if (empty? val)
                        false
                        (or (depends-on? (first val) var frame) (depends-on? (rest val) var frame)))
    :else false))

(declare unify)

(defn extend-if-possible [var val frame]
  (let [val-lhs (frame var)]
    (cond
      val-lhs (unify val-lhs val frame)
      (variable? val) (let [val-rhs (frame val)]
                        (if val-rhs
                          (unify var val-rhs frame)
                          (assoc frame var val)))
      (depends-on? val var frame) nil
      :else (assoc frame var val))))

(defn unify-seqs [lhs-dat rhs-dat frame]
  (cond
    (and (empty? lhs-dat) (empty? rhs-dat)) frame
    (and (= (first lhs-dat) '.) (= (first rhs-dat) '.)) (unify (nth lhs-dat 1) (nth rhs-dat 1) frame)
    (= (first lhs-dat) '.) (unify (nth lhs-dat 1) rhs-dat frame)
    (= (first rhs-dat) '.) (unify lhs-dat (nth rhs-dat 1) frame)
    (or (empty? lhs-dat) (empty? rhs-dat)) nil
    :else (unify (rest lhs-dat) (rest rhs-dat) (unify (first lhs-dat) (first rhs-dat) frame))))

(defn unify [p1 p2 frame]
  (cond
    (nil? frame) frame
    (variable? p1) (extend-if-possible p1 p2 frame)
    (variable? p2) (extend-if-possible p2 p1 frame)
    (and (sequential? p1) (sequential? p2)) (unify-seqs p1 p2 frame)
    (= p1 p2) frame
    :else nil))

;; instantiation

(defn instantiate [expr frame unbound-var-handler]
  (letfn [(instantiate-seq [seqn]
            (cond
              (empty? seqn) '()
              (= (first seqn) '.) (let [cdr (instantiate-main (nth seqn 1))]
                                    (if (sequential? cdr)
                                      cdr
                                      (list '. cdr)))
              :else (cons (instantiate-main (first seqn)) (instantiate-main (rest seqn)))))
          (instantiate-main [expr]
            (cond
              (variable? expr) (let [var (frame expr)]
                                 (if (nil? var)
                                   (unbound-var-handler expr frame)
                                   (instantiate-main var)))
              (and (sequential? expr) (empty? expr)) '()
              (sequential? expr) (instantiate-seq expr)
              :else expr))]
    (instantiate-main expr)))

(defn instantiate-all [expr frames unbound-var-handler]
  (map #(instantiate expr % unbound-var-handler) frames))

;; compound queries

(defn and-query? [query]
  (and (sequential? query) (= (first query) 'and)))

(defn get-and-body [and-query] (rest and-query))

(defn or-query? [query]
  (and (sequential? query) (= (first query) 'or)))

(defn get-or-body [or-query] (rest or-query))

(defn not-query? [query]
  (and (sequential? query) (= (first query) 'not)))

(defn get-not-body [query] (nth query 1))

(defn clojure-pred-query? [query]
  (and (sequential? query) (= (first query) 'clojure-predicate)))

(defn get-clojure-pred-body [clojure-pred-query] (rest clojure-pred-query))

;; eval primitives

(declare ev-query)

(defn find-assertions [query db frame]
  (filter
   (comp not nil?)
   (map
    #(match query % frame)
    (fetch-assertions db query))))

(defn apply-rules  [query db frame]
  (filter
   (comp not nil?)
   (flatmap
    (fn [rule]
      (let [mangled-rule (mangle-rule rule)
            new-frame (unify query (get-conclusion mangled-rule) frame)]
        (if (nil? new-frame)
          '()
          (let [pat (get-rule-body mangled-rule)]
            (if (nil? pat)
              (single-elem-stream new-frame)
              (ev-query pat db (single-elem-stream new-frame)))))))
    (fetch-rules db query))))

(defn ev-simple-query [query db frames]
  (flatmap
   (fn [frame]
     (concat
      (find-assertions query db frame)
      (apply-rules query db frame)))
   frames)) 

(defn ev-and-query [and-query db frames]
  (reduce
   #(ev-query %2 db %1)
   frames
   (get-and-body and-query)))

(defn ev-or-query [or-query db frames]
  (flatmap #(ev-query % db frames) (get-or-body or-query)))

(defn ev-not-query [not-query db frames]
  (let [query (get-not-body not-query)]
    (filter
     #(let [matches (ev-query query db (single-elem-stream %))]
        (empty? matches))
     frames)))

(defn apply-clojure-func [func queries frame]
  (apply
   func
   (map
    (fn [query]
      (instantiate
       query
       frame
       (fn [expr _]
         (throw (ex-info "Unknown variable: " {:var expr})))))
    queries)))

(defn ev-clojure-pred-query [clojure-pred-query _ frames]
  (let [[func-str & queries] (get-clojure-pred-body clojure-pred-query)
        func (eval (read-string func-str))]
    (filter
     #(apply-clojure-func func queries %)
     frames)))

(defn ev-query [query db frames]
  (cond
    (and-query? query) (ev-and-query query db frames)
    (or-query? query) (ev-or-query query db frames)
    (not-query? query) (ev-not-query query db frames)
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
                  results (instantiate-all expr frames (fn [expr _] (unmangle-variable expr)))]
              {:type :query
               :db db
               :results results})))

(defn run-driver-loop [db frames]
  (emit-prompt input-prompt)
  (let [response (ev (read-input) db frames)]
    (display-result response)
    (recur (:db response) (init-frames))))

