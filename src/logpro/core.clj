(ns logpro.core
  (:require [logpro.driver :as dr]
            [clojure.edn :as edn]))

(defn read-data [path]
  (edn/read-string (str "(" (slurp path) ")")))

(defn -main [path & _]
  (let [db (try (dr/init-db (dr/make-db) (read-data path))
                (catch Exception e (do
                                     (println (format "Bad database: %s" (ex-message e)))
                                     (System/exit 1))))]
    (dr/run-driver-loop db (dr/init-frames)))) ;; TODO: frames initialization

