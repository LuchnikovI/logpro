(ns logpro.relation
  (:require
   [logpro.exprs :refer [numerical-op? get-expr-head get-expr-tail numerical-literal? variable?
                         make-subtract-expression]]))

(defn make-constant-relation [val] {:free-term val})

(defn make-single-var-relation [name] {:free-term 0 name 1})

(defn rebuild-relation [relation xf] (into {} xf relation))

(def transduce-relation transduce)

(defn constant-relation? [relation] (and (= (count relation) 1) (:free-term relation)))

(def get-free-term :free-term)

(defn insert-or-merge [merge-fn m key val]
  (if (m key)
    (update m key #(merge-fn % val))
    (assoc m key (merge-fn val))))

(defn scale-relation [factor relation]
  (rebuild-relation relation (map (fn [[k v]] [k (* factor v)]))))

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
  (rebuild-relation relation (filter (fn [[var val]] (or (not= val 0) (= var :free-term))))))

(defn equality-constraint->relation [lhs rhs]
  (let [expr (make-subtract-expression lhs rhs)]
    (-> expr
        ev-to-relation
        remove-zeros-from-relation)))
