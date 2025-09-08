(ns logpro.driver
  (:require [clojure.edn :as edn]
            [logpro.exprs :refer [rule? assertion? get-assertion-body unmangle-variable]]
            [logpro.frames :refer [instantiate-stream init-frames-stream]]
            [logpro.db :refer [add-rule add-assertion]]
            [logpro.ev :refer [ev-query]]))

(def assertion-added-prompt "Assertion added")

(def rule-added-prompt "Rule added")

(def input-prompt ">> Query input:")

(def end-prompt ">> End")

(defn emit-prompt [prompt] (println prompt))

(defn read-input [] (edn/read-string (read-line)))

(defmulti display-result
  (fn [response]
    (:type response)))

(defmethod display-result :assertion-added [_]
  (println)
  (emit-prompt assertion-added-prompt)
  (println))

(defmethod display-result :rule-added [_]
  (println)
  (emit-prompt rule-added-prompt)
  (println))

(defmethod display-result :error [error]
  (println)
  (emit-prompt (:results error))
  (println))

(defmethod display-result :query [response]
  (println)
  (letfn [(helper [response]
            (if-let [current-result (first response)]
              (do (emit-prompt current-result)
                (let [input (read-input)]
                  (if (= input 'stop)
                    (do (emit-prompt end-prompt)
                        nil)
                    (helper (rest response)))))
              (do (emit-prompt end-prompt)
                  (println)
                  nil)))]
    (helper (:results response))))

;; core eval and driver loop

(defn ev [expr db frames]
    (cond
      (assertion? expr) (let [assertion-body (get-assertion-body expr)]
                          (if (rule? assertion-body)
                            {:type :rule-added
                             :db (add-rule db expr)
                             :results nil}
                            {:type :assertion-added
                             :db (add-assertion db expr)
                             :results nil}))
      :else (let [frames (ev-query expr db frames)
                  results (instantiate-stream expr frames (fn [expr _] (unmangle-variable expr)))]
              {:type :query
               :db db
               :results results})))

(defn get-error-result [exception db]
  {:type :error
   :db db
   :results (format "Query was executed with error: %s, data: %s" (ex-message exception) (ex-data exception))})

(defn run-driver-loop [db frames]
  (emit-prompt input-prompt)
  (let [response (try (let [response (ev (read-input) db frames)]
                         (display-result response)
                         response)
                      (catch Exception e (let [response (get-error-result e db)]
                                           (display-result response)
                                           response)))]
    (recur (:db response) (init-frames-stream))))

