(ns logpro.db
  (:require
   [logpro.exprs :refer
    [rule? get-index get-conclusion]]))

(def empty-db
  {:all-assertions []
   :indexed-assertions {}
   :all-rules []
   :indexed-rules {}})

(defn fetch-assertions [db pat]
  (if-let [idx (get-index pat)]
    ((:indexed-assertions db) idx)
    (:all-assertions db)))

(defn fetch-rules [db rule]
  (if-let [idx (get-index rule)]
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

(defn add-assertion-if-indexed [db assertion]
  (if-let [idx (get-index assertion)]
     (add-to-indexed-assertions db idx assertion)
     db))

(defn add-assertion [db assertion]
  (-> db
      (add-assertion-if-indexed assertion)
      (add-to-all-assertions assertion)))

(defn add-to-all-rules [db rule]
  (update db :all-rules #(conj % rule)))

(defn add-to-indexed-rules [db idx assertion]
  (update
   db
   :indexed-rules
   #(insert-or-conj % idx assertion)))

(defn add-rule-if-indexed [db rule]
   (if-let [idx (get-index (get-conclusion rule))]
     (add-to-indexed-rules db idx rule)
     db))

(defn add-rule [db rule]
  (-> db
      (add-rule-if-indexed rule)
      (add-to-all-rules rule)))

(defn init-db [db records]
  (reduce
   (fn [db record]
     (if (rule? record)
       (add-rule db record)
       (add-assertion db record)))
   db
   records))
