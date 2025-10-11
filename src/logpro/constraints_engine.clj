(ns logpro.constraints-engine
  (:require [logpro.exprs :refer [make-subtract-expression numerical-literal? numerical-op?
                                  get-expr-head get-expr-tail variable?]]
            [logpro.frames :refer [get-constraints get-propagated-var propagate-var
                                   update-constraints]]
            [logpro.matching :refer [match]]))

;; relation abstraction

(defn make-constant-relation [val] {:free-term val})

(defn make-single-var-relation [name] {:free-term 0 name 1})

(defn constant-relation? [relation] (= (count relation) 1))

(def get-free-term :free-term)

(def get-seq seq)

(defn insert-or-merge [merge-fn m key val]
  (if (m key)
    (update m key #(merge-fn % val))
    (assoc m key (merge-fn val))))

(defn make-from-seq [s] (into {} s))

(defn scale-relation [factor relation]
  (make-from-seq (map (fn [[k v]] [k (* factor v)]) (get-seq relation))))

(defn get-variables [relation]
  (filter #(not= % :free-term) (keys relation)))

(defn delete-variable [relation var]
  (dissoc relation var))

(defn get-coefficient [relation var]
  (relation var))

(defn merge-pair [merge-fn lhs-rel rhs-rel]
  (reduce
   (fn [lhs-rel [var rhs-val]] (insert-or-merge merge-fn lhs-rel var rhs-val))
   lhs-rel
   rhs-rel))

(defn sum-relations [relations]
  (reduce #(merge-pair + %1 %2) relations))

(defn sub-relations [relations]
  (cond
    (= 1 (count relations)) (sub-relations (cons (make-constant-relation 0) relations))
    (= 2 (count relations)) (merge-pair - (first relations) (second relations))
    :else (throw
           (ex-info
            "Operator \"-\" must be applied to one or two arguments"
            {'arguments-number (count relations)}))))

(defn mul-pair [lhs-rel rhs-rel]
  (cond
    (constant-relation? lhs-rel) (scale-relation (get-free-term lhs-rel) rhs-rel)
    (constant-relation? rhs-rel) (scale-relation (get-free-term rhs-rel) lhs-rel)
    :else (throw (ex-info "Only linear arithmetic expressions are allowed" {}))))

(defn div-pair [lhs-rel rhs-rel]
  (if (constant-relation? rhs-rel)
    (scale-relation (/ 1 (get-free-term rhs-rel)) lhs-rel)
    (throw (ex-info "Only linear arithmetic expressions are allowed" {}))))

(defn mul-relations [relations]
  (reduce #(mul-pair %1 %2) relations))

(defn div-relations [relations]
  (if (= (count relations) 2)
    (div-pair (first relations) (second relations))
    (throw
     (ex-info
      "Operator \"/\" must be applied to two arguments"
      {'arguments-number (count relations)}))))

(defn ev-to-relation [expr]
  (cond
    (numerical-op? expr) (let [head (get-expr-head expr)
                               body (map ev-to-relation (get-expr-tail expr))]
                           (case head
                                + (sum-relations body)
                                - (sub-relations body)
                                * (mul-relations body)
                                / (div-relations body)
                                (throw (ex-info "Unknown numerical operation" {'operation head}))))
    (numerical-literal? expr) (make-constant-relation expr)
    (variable? expr) (make-single-var-relation expr)
    :else (throw (ex-info "Non-numerical input in numerical expression" {'input expr}))))

(defn remove-zeros-from-relation [relation]
  (make-from-seq (filter (fn [[var val]] (or (= var :free-term) (not= val 0))) (get-seq relation))))

(defn equality-constraint->relation [lhs rhs]
  (let [expr (make-subtract-expression lhs rhs)]
    (-> expr
        ev-to-relation
        remove-zeros-from-relation)))

(defn compute-var-coeff-prod [[var coeff] frame]
  (* coeff (get-propagated-var frame var)))

(defn ev-relation [relation frame]
  (let [free-term (:free-term relation)
        var-coeff-stream (filter #(not= (first %) :free-term) (get-seq relation))
        val-stream (map #(compute-var-coeff-prod % frame) var-coeff-stream)]
    (+ free-term (reduce + val-stream))))

(defn equal-zero? [relation frame]
    (= 0 (ev-relation relation frame)))

(defn solve-wrt-var [var relation frame]
  (let [coeff (get-coefficient relation var)]
    (- (/ (ev-relation (delete-variable relation var) frame) coeff))))

;; constraints network abstraction

(defn insert-or-conj [m k v]
  (if (m k)
    (update m k #(conj % v))
    (assoc m k [v])))

(defn insert-constraint [constraints relation]
  (reduce
   (fn [constraints var]
     (insert-or-conj constraints var relation))
   constraints
   (get-variables relation)))

(defn get-relations-given-var [constraints var]
  (constraints var))

(declare awake-relation)

(defn awake-var-assoc-relations [var frame]
  (reduce awake-relation
          frame
          (get-relations-given-var (get-constraints frame) var)))

(defn awake-relation [frame relation]
  (if (nil? frame)
    nil
    (let [unknown-vars (filter (comp not #(get-propagated-var frame %)) (get-variables relation))]
      (cond
        (empty? unknown-vars) (if (equal-zero? relation frame) frame nil)
        (= (count unknown-vars) 1) (let [var (first unknown-vars)
                                         val (solve-wrt-var var relation frame)
                                         frame (propagate-var frame var val)
                                         frame (match var val frame)]
                                     (awake-var-assoc-relations var frame))
        :else frame))))

(defn add-equality-constraint [frame lhs rhs]
  (let [relation (equality-constraint->relation lhs rhs)]
    (-> frame
        (update-constraints #(insert-constraint % relation))
        (awake-relation relation))))
