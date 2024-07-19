(ns fin-data.db-io
  (:require [clojure.java.io :as io]
            [sys-loader.bootstrap :as sys-boot]
            [next.jdbc.result-set :as rs]
            [next.jdbc :as jdbc]
            [fin-data.md5 :refer [md5]])
  (:import [java.text SimpleDateFormat]
           [java.time.format DateTimeFormatter FormatStyle]
           [java.time LocalDate]
           [java.sql Date]))

(defn to-sql-date
  "Convert date string of the format April 09, 2024 to a 
   java.sql Date object."
  [date-str]
  (-> FormatStyle/LONG
      DateTimeFormatter/ofLocalizedDate
      (.parse date-str)
      LocalDate/from
      Date/valueOf))

(defn data-src
  "Grab the datasource from the sys-loader context"
  []
  (-> @sys-boot/sys-state
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

(defn with-conn [f]
  {:pre [(fn? f)]}
  (with-open [conn (jdbc/get-connection (data-src))]
    (f conn)))

;; Note - jdbc.next has many options regarding the shape of result sets.
;; Currently using as-unqualified-lower-map as this suits my taste at the
;; moment. 
(defn select-import
  "fetch imported data"
  []
  (with-conn
    (fn [conn]
      (jdbc/execute! conn
                     [(sql-text :select-boa-import)]
                     {:builder-fn rs/as-unqualified-lower-maps}))))

(defn calc-md5
  "Calc MD5 sum for adding to import table. This is needed to generate a 
   primary key for the BOA statements. The CSV download does not provide
   any type of unique row identifier."
  []
  (map (fn [x]
         (let [{:keys [id posting_date description amount]} x]
           {:id id :md5 (md5 (str posting_date description amount))}))
       (select-import)))

;; TODO - this can be optimized a bit
(defn update-md5! []
  (doseq [csum (calc-md5)]
    (let [{:keys [id md5]} csum]
      (with-conn
        (fn [conn]
          (jdbc/execute!
           conn
           [(sql-text :update-md5) md5 id]))))))

(defn import-csv
  "Import BOA CSV file into import table
   finkratzen.boa_import"
  []
  (with-open [conn (jdbc/get-connection (data-src))]
    (jdbc/execute-one! conn ["drop table if exists finkratzen.boa_import"])
    (jdbc/execute-one! conn
                       [(csv-import-sql)])))

(defn insert-checking [bank-txn]
  (with-open [conn (jdbc/get-connection (data-src))]
    (let [{:keys [amt merchant on]} bank-txn
          chksum (md5 (str amt merchant on))]
      (jdbc/execute-one! conn
                         [(sql-text :insert-checking)
                          chksum
                          (to-sql-date on)
                          amt
                          merchant]))))

(comment
  *e
  ;; example formatter
  ;; https://www.baeldung.com/java-datetimeformatter

  ;; This was an issue causing posting date to be wrong. 
  ;; Apparently SimpleDateFormat is broken here, month is 
  ;; always January.
  (-> (SimpleDateFormat. "MMMMM DD, yyyy" java.util.Locale/US)
      (.parse "April 09, 2024"))

  ;;DateTimeFormatter.ofLocalizedDate (FormatStyle.LONG) .format

  (to-sql-date  "April 09, 2025")

  (.parse (DateTimeFormatter/ofLocalizedDate FormatStyle/LONG) "April 09, 2024")

  (update-md5!)
  (select-import)
  (first (calc-md5))
  (calc-md5)

  (time (calc-md5))

  (time select-import)
  (sql-text :update-md5)
  (sql-text :insert-checking)
  (sql-text :select-boa-import)
  (data-src)
  (import-csv)
  (csv-import-sql)

  (time (import-csv))

  (System/getProperty "user.home")
  (data-src)
  ;;
  )