(ns logpro.frames
  (:require [logpro.exprs :refer
             [variable? vararg-head? get-varargs empty-expr? compound-expr?
              empty-expr make-pair-expression vararg-symbol extend-compound-expr
              get-expr-head get-expr-tail numerical-literal?]]
            [logpro.relation :refer [get-variables equality-constraint->relation
                                     delete-variable get-coefficient transduce-relation]]))

(declare get-propagated-var get-constraints propagate-var update-constraints
         match unify)

;; relations evaluation

(defn ev-single-term [[var coeff] frame]
  (* coeff (get-propagated-var frame var)))

(defn ev-relation [relation frame]
  (transduce-relation
   (comp
    (filter (fn [[k _]] (not= k :free-term)))
    (map #(ev-single-term % frame)))
   +
   (:free-term relation)
   relation))

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
  (if (nil? frame)
    nil
    (reduce awake-relation
            frame
            (get-relations-given-var (get-constraints frame) var))))

(defn awake-relation [frame relation]
  (if (nil? frame)
    nil
    (let [unknown-vars (filter (comp not #(get-propagated-var frame %)) (get-variables relation))]
      (cond
        (empty? unknown-vars) (if (equal-zero? relation frame) frame nil)
        (= (count unknown-vars) 1) (let [var (first unknown-vars)
                                         val (solve-wrt-var var relation frame)]
                                     (as-> frame $
                                         (propagate-var $ var val)
                                         (match var val $)
                                         (awake-var-assoc-relations var $)))
        :else frame))))

;; frame initialization

(def empty-constraints {})

(def empty-propagated-vars {})

(def empty-frame {:constraints empty-constraints
                  :propagated-variables empty-propagated-vars})

;; single frame operations

(defn get-binding [frame var]
  (frame var))

(defn add-equality-constraint [frame lhs rhs]
  (let [relation (equality-constraint->relation lhs rhs)]
    (-> frame
        (update-constraints #(insert-constraint % relation))
        (awake-relation relation))))

(defn insert-to-constraints [frame var val]
  (if (or (variable? val) (numerical-literal? val))
    (add-equality-constraint frame var val)
    frame))

(defn insert-binding [frame var val]
  (-> frame
      (assoc var val)
      (insert-to-constraints var val)))

(defn invalid-frame? [frame] (nil? frame))

(def invalid-frame nil)

(defn get-constraints [frame]
  (:constraints frame))

(defn get-propagated-var [frame var]
  ((:propagated-variables frame) var))

(defn propagate-var [frame var val]
  (update frame :propagated-variables #(assoc % var val)))

(defn update-constraints [frame func]
  (update frame :constraints func))

;; stream of frames operations

(defmacro get-single-elem-stream [expr]
  `(lazy-seq (cons ~expr nil)))

(def empty-frames-stream '())

(defn init-frames-stream []
  (get-single-elem-stream empty-frame))

(defn filter-invalid-frames [frames-stream]
  (filter (comp not invalid-frame?) frames-stream))

(defn intrlv [seq-of-seqs]
  (when (some seq seq-of-seqs)
    (concat
     (keep first seq-of-seqs)
     (lazy-seq (intrlv (map rest seq-of-seqs))))))

(defn flatmap [func iter]
  (lazy-seq (->> iter
                (map func)
                intrlv)))

;; instantiation of an expression given a frame

(declare instantiate)

(defn instantiate-compound [compound-expr frame unbound-var-handler]
  (cond
    (empty-expr? compound-expr) empty-expr
    (vararg-head? compound-expr) (let [varargs (instantiate
                                                (get-varargs compound-expr)
                                                frame
                                                unbound-var-handler)]
                                   (if (compound-expr? varargs)
                                     varargs
                                     (make-pair-expression vararg-symbol varargs)))
    :else (extend-compound-expr
           (instantiate (get-expr-head compound-expr) frame unbound-var-handler)
           (instantiate (get-expr-tail compound-expr) frame unbound-var-handler))))

(defn instantiate [expr frame unbound-var-handler]
  (cond
    (variable? expr) (let [val (get-binding frame expr)]
                       (if val
                         (recur val frame unbound-var-handler)
                         (unbound-var-handler expr frame)))
    (compound-expr? expr) (instantiate-compound expr frame unbound-var-handler)
    :else expr))

(defn instantiate-stream [expr frames unbound-var-handler]
  (map #(instantiate expr % unbound-var-handler) frames))

;; matching

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
