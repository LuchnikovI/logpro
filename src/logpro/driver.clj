(ns logpro.driver
  (:require [logpro.ev :refer [ev]]
            [logpro.frames :refer [init-frames-stream]]))

(defn read-forms [] (read {:eof nil} *in*))

(def assertion-added-prompt 'assertion-added)

(def rule-added-prompt 'rule-added)

(def input-prompt ";; Query input:")

(def result-prompt ";; Result:")

(def end-prompt ";; End")

(def end-of-file-prompt ";; Bye!")

(defn emit-prompt [prompt] (println prompt))

(defmulti display-result :type)

(defmethod display-result :assertion-added [_]
  (emit-prompt result-prompt)
  (emit-prompt assertion-added-prompt)
  (emit-prompt end-prompt))

(defmethod display-result :rule-added [_]
  (emit-prompt result-prompt)
  (emit-prompt rule-added-prompt)
  (emit-prompt end-prompt))

(defmethod display-result :error [result]
  (emit-prompt result-prompt)
  (emit-prompt (:error result))
  (emit-prompt end-prompt))

(defmethod display-result :query [result]
  (emit-prompt result-prompt)
  (some
   (fn [curr-result]
     (emit-prompt curr-result))
   (:results result))
  (emit-prompt end-prompt))

(defmethod display-result :end-of-file [_]
  (emit-prompt end-of-file-prompt))

(defn get-error-result [ex]
  {:type :error
   :error (list 'error (list 'message (ex-message ex)) (list 'data (ex-data ex)))})

(defn read-eval-print-succ [frames]
  (let [result (ev (read-forms) frames)]
    (display-result result)
    result))

(defn read-eval-print-fail [ex]
  (let [result (get-error-result ex)]
    (display-result result)
    result))

(defn read-eval-print [frames]
  (try
    (read-eval-print-succ frames)
    (catch Exception ex (read-eval-print-fail ex))))

(defn run-driver-loop [frames]
  (emit-prompt input-prompt)
  (let [result (read-eval-print frames)]
    (case (:type result)
      :end-of-file nil
      (recur (init-frames-stream)))))

