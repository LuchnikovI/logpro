(ns logpro.db
  (:require
   [logpro.state :refer [state]]
   [logpro.exprs :refer
    [rule? get-index get-conclusion]]))

(defn fetch-assertions [pat]
  (if-let [idx (get-index pat)]
    ((:indexed-assertions @state) idx)
    (:all-assertions @state)))

(defn fetch-rules [rule]
  (if-let [idx (get-index rule)]
    ((:indexed-rules @state) idx)
    (:all-rules @state)))

(defn insert-or-conj [m idx elem]
  (if (contains? m idx)
    (update m idx #(conj % elem))
    (assoc m idx [elem])))

(defn add-to-all-assertions! [assertion]
  (swap! state update :all-assertions #(conj % assertion)))

(defn add-to-indexed-assertions! [idx assertion]
  (swap!
   state
   update
   :indexed-assertions
   #(insert-or-conj % idx assertion)))

(defn add-assertion-if-indexed! [assertion]
  (when-let [idx (get-index assertion)]
     (add-to-indexed-assertions! idx assertion)))

(defn add-assertion! [assertion]
  (add-assertion-if-indexed! assertion)
  (add-to-all-assertions! assertion))

(defn add-to-all-rules! [rule]
  (swap! state update :all-rules #(conj % rule)))

(defn add-to-indexed-rules! [idx assertion]
  (swap!
   state
   update
   :indexed-rules
   #(insert-or-conj % idx assertion)))

(defn add-rule-if-indexed! [rule]
   (when-let [idx (get-index (get-conclusion rule))]
     (add-to-indexed-rules! idx rule)))

(defn add-rule! [rule]
  (add-rule-if-indexed! rule)
  (add-to-all-rules! rule))

(defn init-db! [records]
  (doseq [record records]
    (if (rule? record)
       (add-rule! record)
       (add-assertion! record))))

