(ns logpro.unification
  (:require
   [logpro.exprs :refer [variable? get-expr-head get-expr-tail empty-expr?
                         compound-expr? vararg-head? get-varargs]]
   [logpro.frames :refer [get-binding insert-binding invalid-frame invalid-frame?]]))

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

(defn unify-with-every [p ps frame]
  (cond
    (variable? ps) (if-let [ps-deref (get-binding frame ps)]
                     (unify-with-every p ps-deref frame)
                     invalid-frame)
    (compound-expr? ps) (map #(unify p % frame) ps)
    :else invalid-frame))
    
