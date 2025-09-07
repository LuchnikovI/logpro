(ns logpro.frames
  (:require [logpro.exprs :refer [variable?]]))

;; single frame operations

(defn get-binding [frame var]
  (frame var))

(defn insert-binding [frame var val]
  (assoc frame var val))

(defn invalid-frame? [frame] (nil? frame))

(def invalid-frame nil)

;; stream of frames operations

(defmacro get-single-elem-stream [expr]
  `(lazy-seq (cons ~expr nil)))

(def empty-frames-stream '())

(defn init-frames-stream []
  (get-single-elem-stream {}))

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

(defn instantiate-seq [seqn frame unbound-var-handler]
  (cond
    (empty? seqn) '()
    (= (first seqn) '.) (let [cdr (instantiate (nth seqn 1) frame unbound-var-handler)]
                          (if (sequential? cdr)
                            cdr
                            (list '. cdr)))
    :else (cons
           (instantiate (first seqn) frame unbound-var-handler)
           (instantiate (rest seqn) frame unbound-var-handler))))

(defn instantiate [expr frame unbound-var-handler]
  (cond
    (variable? expr) (let [val (frame expr)]
                       (if val
                         (recur val frame unbound-var-handler)
                         (unbound-var-handler expr frame)))
    (sequential? expr) (instantiate-seq expr frame unbound-var-handler)
    :else expr))

(defn instantiate-stream [expr frames unbound-var-handler]
  (map #(instantiate expr % unbound-var-handler) frames))

