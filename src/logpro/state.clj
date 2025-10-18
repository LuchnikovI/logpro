(ns logpro.state)

(def state (atom {:all-assertions []
                  :indexed-assertions {}
                  :all-rules []
                  :indexed-rules {}
                  :mangling-counter 0
                  :variables {}}))

(defn get-mangling-counter! []
  (let [curr-counter (:mangling-counter @state)]
    (swap! state update :mangling-counter inc)
    curr-counter))

(defn get-variable-id! [smthng]
  (if-let [iden (get-in [:variables smthng] @state)]
    iden
    (let [new-iden (count (:variables @state))]
      (swap! state update :variables assoc smthng new-iden)
      new-iden)))

