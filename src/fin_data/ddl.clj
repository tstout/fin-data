(ns fin-data.ddl
  (:require [sys-loader.bootstrap :as sys-boot]))

(defn init-schema [run-ddl]
  (run-ddl "init-schema"))

(defn checking-schema [run-ddl]
  (run-ddl "checking"))

(defn exec-ddl [migrate-fn]
  {:pre [(fn? migrate-fn)]}
  (doseq [ddl-fn [#'init-schema
                  #'checking-schema
                  #_"Add additional ddl fns here"]]
    (migrate-fn ddl-fn)))

(comment
  *e

  (-> @sys-boot/sys-state
      :sys/migrations
      exec-ddl)
  ;;
  )