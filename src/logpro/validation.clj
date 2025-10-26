(ns logpro.validation)

(declare validate-dot-position-rf validate-query validate-arithmetic-exprs validate-simple-query)

(def keywords #{'not 'range 'is 'eq 'collect 'in 'and 'or '= '<= '>= '< '> 'clojure-predicate})

(defn validation-bug [] (throw (ex-info "Validation bug" {})))

(defmacro defmethod-list [name disp-vals args & body]
  `(do ~@(map (fn [disp-val] `(defmethod ~name ~disp-val ~args ~@body)) disp-vals)))

(defn- wrap-special-form-error [error form]
  {:invalid-special-form form :error error})

(defn- first-question-mark? [symbol]
  (= (first (str symbol)) \?))

(defn variable? [expr]
  (and (symbol? expr) (first-question-mark? expr)))

(defn- enumerate-xf [rf]
  (let [counter (atom 0)]
    (fn
      ([] (rf))
      ([state] (rf state))
      ([state elem] (let [num @counter] (swap! counter inc) (rf state [num elem]))))))

(def reduce-enumerate (partial transduce enumerate-xf))

(defn- validate-len [form length]
  (when (cond
          (number? length) (not= length (count form))
          (or (set? length) (fn? length)) (not (length (count form))))
    {:invalid-length-of form}))

(defn- make-validate-seq-rf [validation-func]
  (fn
    ([] [])
    ([state] (seq state))
    ([state [pos elem]]
     (if-let [error (validation-func elem)]
       (conj state {:position pos :error error})
       state))))

(defn validate-number [number]
  (when-not (or (number? number) (variable? number))
    {:not-a-number number}))

(def validate-number-rf
  (make-validate-seq-rf validate-number)) 

(defn- validate-numbers [numbers]
  (seq (reduce-enumerate validate-number-rf numbers)))

;; arithmetic form validation

(defn- validate-arithmetic [expr]
  (cond
    (or (variable? expr) (number? expr) (float? expr)) nil
    (and (list? expr) (seq expr)) (let [arithm-exprs (rest expr)]
                                    (case (first expr)
                                      + (or
                                         (validate-len expr #(> % 1))
                                         (validate-arithmetic-exprs arithm-exprs))
                                      * (or
                                         (validate-len expr #(> % 2))
                                         (validate-arithmetic-exprs arithm-exprs))
                                      - (or
                                         (validate-len expr 3)
                                         (validate-arithmetic-exprs arithm-exprs))
                                      / (or
                                         (validate-len expr 3)
                                         (validate-arithmetic-exprs arithm-exprs))
                                      {:unknown-arithmetic-op (first expr)}))
    :else {:not-aritmetic expr}))

(def validate-arithmetic-rf
  (make-validate-seq-rf validate-arithmetic))

(defn- validate-arithmetic-exprs [exprs]
  (seq (reduce-enumerate validate-arithmetic-rf exprs)))

;; special forms validation

(defmulti validate-special-form
  (fn [special-form] (first special-form)))

(defmethod validate-special-form 'not [form]
  (some-> (or
           (validate-len form 2)
           (validate-query (second form)))
          (wrap-special-form-error form)))

(defmethod validate-special-form 'range [form]
  (some-> (or
           (validate-len form #{2 3 4 5})
           validate-numbers)
          (wrap-special-form-error form)))

(defmethod validate-special-form 'is [form]
  (some-> (or
           (validate-len form 3)
           (let [[_ lhs rhs] form]
             (or
              (validate-number lhs)
              (validate-arithmetic rhs))))
          (wrap-special-form-error form)))

(defmethod validate-special-form 'eq [form]
  (some-> (or
           (validate-len form 3)
           (let [[_ lhs rhs] form]
             (or
              (validate-simple-query lhs)
              (validate-simple-query rhs))))
          (wrap-special-form-error form)))

(defn- validate-list-or-variable [form]
  (or
    (when-not (or (list? form) (variable? form))
      {:not-list-or-variable form})
    (validate-simple-query form)))

(defmethod validate-special-form 'in [form]
  (some-> (or
           (validate-len form 3)
           (let [[_ lhs rhs] form]
             (or
              (validate-simple-query lhs)
              (validate-list-or-variable rhs))))
          (wrap-special-form-error form)))

(defmethod validate-special-form 'and [form]
  (some-> (or
           (validate-len form #(>= % 3))
           (let [[_ & args] form]
             (reduce-enumerate (make-validate-seq-rf validate-query) args)))
          (wrap-special-form-error form)))

(defmethod validate-special-form 'or [form]
  (some-> (or
           (validate-len form #(>= % 3))
           (let [[_ & args] form]
             (reduce-enumerate (make-validate-seq-rf validate-query) args)))
          (wrap-special-form-error form)))

(defmethod-list validate-special-form ['= '<= '>= '< '>] [form]
  (some-> (or
           (validate-len form 3)
           (let [[_ lhs rhs] form]
             (or
              (validate-arithmetic lhs)
              (validate-arithmetic rhs))))
          (wrap-special-form-error form)))

(defn- validate-string [s]
  (when-not (string? s)
    {:not-a-string s}))

;; TODO: fix shift of the position
(defmethod validate-special-form 'clojure-predicate [form]
  (some-> (or
           (validate-len form #(>= % 2))
           (let [[_ clj-fn & args] form]
             (or
               (validate-string clj-fn)
               (reduce-enumerate (make-validate-seq-rf validate-simple-query) args))))
          (wrap-special-form-error form)))

(defn- validate-collect-3 [[_ query bag]]
  (or
   (validate-simple-query query)
   (validate-list-or-variable bag)))

(defn- validate-collect-4 [[_ teamplate query bag]]
  (or
   (validate-simple-query teamplate)
   (validate-simple-query query)
   (validate-list-or-variable bag)))

(defn- validate-collect-5 [[_ teamplate label query bag]]
  (or
   (validate-simple-query teamplate)
   (validate-simple-query label)
   (validate-simple-query query)
   (validate-list-or-variable bag)))

(defmethod validate-special-form 'collect [form]
  (some-> (case (count form)
            3 (validate-collect-3 form)
            4 (validate-collect-4 form)
            5 (validate-collect-5 form)
            {:invalid-length-of form})
          (wrap-special-form-error form)))

;; query validation

(defn- wrap-compound-simple-query-error [error query]
  {:invalid-compound-query query :error error})

(declare validate-simple-query-rf)

(defn- validate-compound-simple-query [simple-query]
  (some-> (or
           (transduce identity validate-dot-position-rf simple-query)
           (reduce-enumerate validate-simple-query-rf simple-query))
          (wrap-compound-simple-query-error simple-query)))

(defn- elementary-query? [query]
  (or (number? query) (float? query) (symbol? query)))

(defn- validate-simple-query [simple-query]
  (cond
    (elementary-query? simple-query) nil
    (list? simple-query) (if (keywords (first simple-query))
                           {:not-a-simple-query simple-query}
                           (validate-compound-simple-query simple-query))
    :else {:invalid-simple-query simple-query}))

(def validate-simple-query-rf
  (make-validate-seq-rf validate-simple-query))

(declare validate-clause)

(defn- wrap-assert-error [error query]
  (list 'assert-error query 'error error))

(defn validate-query [query]
  (cond (and (list? query) (keywords (first query))) (validate-special-form query)
        (and (list? query) (= 'assert! (first query)))
        (some-> (or (validate-len query 2)
                    (validate-clause (rest query)))
                (wrap-assert-error query))
        :else (validate-simple-query query)))

;; db validation

(defn- validate-dot-position-rf
  ([] :regular)
  ([state]
   (cond
     (or (symbol? state) (sequential? state)) state
     (= state :after-dot) :terminal-dot
     :else nil))
  ([state clause]
   (case state
     :regular (if (= clause '.) :after-dot :regular)
     :after-dot (if (= clause '.) 'repeated-dot :must-stop)
     :must-stop 'dot-followed-by-multiple-clauses
     state)))

(declare validate-simple-clauses-rf)

(defn- wrap-compound-clause-error [error compound-clause]
  {:invalid-clause compound-clause :error error})

(defn- validate-compound-clause [compound-clause]
  (some-> (or
           (transduce identity validate-dot-position-rf compound-clause)
           (reduce-enumerate validate-simple-clauses-rf compound-clause))
          (wrap-compound-clause-error compound-clause)))

(defn- validate-simple-clause [clause]
  (cond
    (symbol? clause) (when (first-question-mark? clause) {:variable-in-clause clause})
    (number? clause) nil
    (list? clause) (validate-compound-clause clause)
    :else {:unknown-simple-clause clause}))

(def validate-simple-clauses-rf
  (make-validate-seq-rf validate-simple-clause))

(defn- wrap-rule-error [error rule]
  {:invalid-rule rule :error error})

(defn- validate-rule [rule]
  (some-> (or
           (validate-len rule #{2 3})
           (let [[_ sign body] rule]
             (or
              (validate-compound-simple-query sign)
              (when body (validate-query body)))))
          (wrap-rule-error rule)))

(defn- validate-clause [clause]
  (if (and (list? clause) (= (first clause) 'rule))
    (validate-rule clause)
    (validate-simple-clause clause)))

(defn validate-db [db]
  (seq (filter (comp not nil?) (map validate-clause db))))
  
