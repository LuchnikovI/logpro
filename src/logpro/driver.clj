(ns logpro.driver
  (:require [clojure.pprint :refer [pprint]]
            [logpro.ev :refer [ev]]
            [logpro.frames :refer [init-frames-stream]]
            [logpro.validation :refer [validate-query]]))

(defn read-query [] (read {:eof nil} *in*))

(def assertion-added-prompt 'assertion-added)

(def rule-added-prompt 'rule-added)

(def input-prompt ";; Query input:")

(def result-prompt ";; Result:")

(def end-prompt ";; End")

(def end-of-file-prompt ";; Bye!")

(defmulti display-result :type)

(defmethod display-result :assertion-added [_]
  (println result-prompt)
  (println assertion-added-prompt)
  (println end-prompt))

(defmethod display-result :rule-added [_]
  (println result-prompt)
  (println rule-added-prompt)
  (println end-prompt))

(defmethod display-result :error [result]
  (println result-prompt)
  (pprint (:error result))
  (println end-prompt))

(defmethod display-result :query [result]
  (println result-prompt)
  (some
   (fn [curr-result]
     (println curr-result))
   (:results result))
  (println end-prompt))

(defmethod display-result :end-of-file [_]
  (println end-of-file-prompt))

(defn get-error-result [ex]
  {:type :error
   :error (list 'error (list 'message (ex-message ex)) (list 'data (ex-data ex)))})

(defn eval-print-succ [query frames]      
  (let [result (ev query frames)]
    (display-result result)
    result))

(defn print-runtime-error [ex]
  (let [result (get-error-result ex)]
    (display-result result)
    result))

(defn print-validation-error [validation-error]
  (let [result {:type :error :error validation-error}]
    (display-result result)
    result))

(defn read-eval-print [frames]
  (let [query (read-query)]
    (if (nil? query)
      {:type :end-of-file}
      (if-let [validation-error (validate-query query)]
        (print-validation-error validation-error)
        (try
          (eval-print-succ query frames)
          (catch Exception ex (print-runtime-error ex)))))))

(defn run-driver-loop [frames]
  (println input-prompt)
  (let [result (read-eval-print frames)]
    (case (:type result)
      :end-of-file nil
      (recur (init-frames-stream)))))

