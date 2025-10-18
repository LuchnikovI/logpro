(ns logpro.core
  (:require
   [logpro.frames :refer [init-frames-stream]]
   [logpro.db :refer [init-db!]]
   [logpro.driver :as dr]
   [clojure.edn :as edn]))

(defn read-data [path]
  (edn/read-string (str "(" (slurp path) ")")))

(defn -main [db-path]
  (try (init-db! (read-data db-path))
       (catch Exception e (do
                            (println (format "Bad database: %s" (ex-message e)))
                            (System/exit 1))))
  (dr/run-driver-loop (init-frames-stream)))
