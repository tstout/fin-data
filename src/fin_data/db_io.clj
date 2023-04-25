(ns fin-data.db-io
  (:require [clojure.java.io :as io]
            [sys-loader.core :as sys]
            [next.jdbc :as jdbc]))


(defn data-src []
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

(defn csv-import-sql []
  (-> (sql-text :csv-read)
      (.replace "{{file-name}}" 
                (str (System/getProperty "user.home") "/Downloads/stmt.csv"))))


(defn import-csv []
  (with-open [conn (jdbc/get-connection (data-src))]
      (jdbc/execute-one! conn
                     [(csv-import-sql)])))


(comment
  (sql-text :csv-read)
  (data-src)
  (import-csv)
  (csv-import-sql)

  (System/getProperty "user.home")



  ;;
  )