(ns fin-data.db-io
  (:require [clojure.java.io :as io]
            [sys-loader.core :as sys]
            [next.jdbc :as jdbc]))

(defn data-src
  "Grab the datasource from the sys-loader context"
  []
  (-> @sys/sys-state
      :sys/db
      :data-source))

(def sql-text
  "Load SQL text from resource. SQL resource file must exist at 
   sql/queries/<q-name>.sql"
  (memoize
   (fn [q-name]
     (-> (str "sql/queries/" (name q-name) ".sql")
         io/resource
         slurp))))

(defn csv-import-sql
  "Load the sql for csv import. This is a bit of a hack
   as I could not get ? jdbc placeholder to work in this case.
   The csv file to import is expected to be at ~/Downloads/stmt.csv"
  []
  (-> (sql-text :csv-read)
      (.replace "{{file-name}}"
                (str (System/getProperty "user.home") "/Downloads/stmt.csv"))))

(defn import-csv
  "Import BOA CSV file into import table
   finkratzen.boa_import"
  []
  (with-open [conn (jdbc/get-connection (data-src))]
    (jdbc/execute-one! conn ["drop table if exists finkratzen.boa_import"])
    (jdbc/execute-one! conn
                       [(csv-import-sql)])))


(comment
  (sql-text :csv-read)
  (data-src)
  (import-csv)
  (csv-import-sql)

  (time (import-csv))

  (System/getProperty "user.home")



  ;;
  )