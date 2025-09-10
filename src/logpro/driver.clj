(ns logpro.driver
  (:require [clojure.edn :as edn]
            [logpro.frames :refer [init-frames-stream]]
            [logpro.ev :refer [ev]]))

(def assertion-added-prompt "Assertion added")

(def rule-added-prompt "Rule added")

(def input-prompt ">> Query input:")

(def end-prompt ">> End")

(defn emit-prompt [prompt] (println prompt))

(defn read-input [] (edn/read-string (read-line)))

(defmulti display-result :type)

(defmethod display-result :assertion-added [_]
  (println)
  (emit-prompt assertion-added-prompt)
  (println))

(defmethod display-result :rule-added [_]
  (println)
  (emit-prompt rule-added-prompt)
  (println))

(defmethod display-result :error [result]
  (println)
  (emit-prompt (:error result))
  (println))

(defmethod display-result :query [result]
  (println)
  (some
   (fn [curr-result]
     (emit-prompt curr-result)
     (let [input (read-input)]
       (= input 'stop)))
   (:results result))
  (emit-prompt end-prompt))

(defn run-driver-loop [db frames]
  (emit-prompt input-prompt)
  (let [prompt (delay (read-input)) ;; delay it to try catch inside the ev function
        result (ev prompt db frames)]
    (display-result result)
    (recur (:db result) (init-frames-stream))))

