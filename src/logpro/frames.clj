(ns logpro.frames
  (:require [logpro.exprs :refer
             [variable? vararg-head? get-varargs empty-expr? compound-expr?
              empty-expr make-pair-expression vararg-symbol extend-compound-expr
              get-expr-head get-expr-tail numerical-literal? unmangle-variable
              get-label-id! attach-postfix]]))

;; single frame operations

(def empty-constraints {}) ;; must be in constraints_engine.clj, but put here to not struggle with circ. deps.

(def empty-propagated-vars {}) ;; must be in constraints_engine.clj, but put here to not struggle with circ. deps.

(def empty-frame {:constraints empty-constraints :propagated-variables empty-propagated-vars})

(defn get-binding [frame var]
  (frame var))

;; TODO: fix circular deps in a normal way
(defn insert-to-constraints [frame var val]
  (if (or (variable? val) (numerical-literal? val))
    ((requiring-resolve 'logpro.constraints-engine/add-equality-constraint) frame var val)
    frame))

(defn insert-binding [frame var val]
  (-> frame
      (assoc var val)
      (insert-to-constraints var val)))

(defn invalid-frame? [frame] (nil? frame))

(def invalid-frame nil)

(defn get-constraints [frame] (:constraints frame))

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

;; variables in the second instantiation are labeled
;; according to the first expr instantiation
(defn double-instantiate-stream [expr frames]
  (let [exprs-inst (map #(instantiate expr % (fn [var _] (unmangle-variable var))) frames)]
    (map
     #(instantiate
       %1
       %2
       (fn [var _]
         (attach-postfix var "-" (get-label-id! %1))))
     exprs-inst
     frames)))
