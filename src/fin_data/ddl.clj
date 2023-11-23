(ns fin-data.ddl
  (:require [sys-loader.core :as sys]))

(defn init-schema [run-ddl]
  (run-ddl "init-schema"))

(defn exec-ddl [migrate-fn]
  {:pre [(fn? migrate-fn)]}
  (doseq [ddl-fn [#'init-schema
                  #_"Add additional ddl fns here"]]
    (migrate-fn ddl-fn)))

(comment
  *e

  (-> @sys/sys-state
      :sys/migrations
      exec-ddl)
  ;;
  )