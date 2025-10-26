(ns logpro.core
  (:require
   [logpro.frames :refer [init-frames-stream]]
   [logpro.db :refer [init-db!]]
   [logpro.driver :as dr]
   [logpro.validation :refer [validate-db]]
   [clojure.edn :as edn]
   [clojure.pprint :refer [pprint]]))

(defn read-data [path]
  (edn/read-string (str "(" (slurp path) ")")))

(defn -main [db-path]
  (try (let [raw-db (read-data db-path)]
         (if-let [error (validate-db raw-db)]
           (do
             (pprint (list 'invalid-database error))
             (System/exit 1))
           (init-db! raw-db)))
       (catch Exception e (do
                            (pprint (list 'invalid-database (ex-message e)))
                            (System/exit 1))))
  (dr/run-driver-loop (init-frames-stream)))
