#!/usr/bin/env bb

(ns run-tests
  (:require [babashka.process :refer [sh]]
            [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def empty-cache {})

(defn insert [cache elem]
  (if (cache elem)
    (update cache elem inc)
    (assoc cache elem 1)))

(defn extract [cache elem]
  (let [counts (cache elem)]
    (if counts
      (if (> counts 1)
        (update cache elem dec)
        (dissoc cache elem))
      (throw (ex-info "Output not found among expected results!" {'unexpected-output elem})))))

(def empty-cache? empty?)

(def script-dir
  (-> *file*
      fs/canonicalize
      fs/parent))

(def prompts-dir
  (fs/file script-dir "prompts"))

(def results-dir
  (fs/file script-dir "results"))

(defn run-logpro [db-file prompt-file]
  (edn/read-string (str "(" (:out (sh {:in prompt-file} "lein run" db-file)) ")")))

(defn read-results [results-file]
  (reduce
   insert
   empty-cache
   (edn/read-string (str "(" (slurp results-file) ")"))))

(def dbs-dir
  (-> script-dir
      fs/parent
      (fs/path "examples")))

(doseq [db-file (fs/list-dir dbs-dir)]
  (let [db-file-name (fs/file-name db-file)
        prompt-file (fs/file prompts-dir (str db-file-name "_prompts"))
        result-file (fs/file results-dir (str db-file-name "_result.edn"))]
    (when (fs/exists? prompt-file)
      (println "Testing " (fs/file-name db-file) " example")
      (let [outputs (run-logpro db-file prompt-file)
            results (read-results result-file)
            remaining-results (reduce
                               extract
                               results
                               outputs)]
        (when (not (empty-cache? remaining-results))
          (throw (ex-info "Not all outputs were generated!" {'lost-outputs remaining-results}))))
      (println (fs/file-name db-file) ": OK"))))
