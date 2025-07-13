(ns fin-data.boa-parse
  (:require [clojure-mail.core :refer [search-inbox]]
            [clojure-mail.gmail :as gmail]
            [clojure.tools.logging :as log]
            [clojure-mail.message :as message]
            [fin-data.db-io :refer [insert-checking]]
            [clojure-mail.parser :refer [html->text]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string :refer [includes? starts-with?]]
            [fin-data.creds :refer [fetch-account]]
            [fin-data.timers :refer [periodic-fn]])
  (:import [java.text SimpleDateFormat]))

(defn legacy-date
  "Create a java.util.Date from the string of dd-MMM-yyyy. This is legacy
  in that it returns a java.util.Date (this what the clojure-mail lib requires)"
  [date-as-str]
  (-> (SimpleDateFormat. "dd-MMM-yyyy")
      (.parse date-as-str)))

(defn find-in-inbox
  "Basic convenience fn for searching an inbox. Note: this is an
   expensive operation on a large inbox."
  [search-term m]
  (let [{:keys [user pass]} m
        msgs (-> (gmail/store user pass)
                 (search-inbox search-term))]
    (doall
     (map message/read-message msgs))))

(defn recent-boa
  "Expensive operation, given the typical size of my inbox. Returns a 
   future so that interaction in a repl won't get blocked for a long 
   period of time. The default time frame is process emails from last 24 hours.
   Provide a second argument of a string representing the up-to date in the form
   dd-MM-YYY"
  ([m]
   (recent-boa m :yesterday))
  ([m received-after-date]
   (let [after-date (if (keyword? received-after-date)
                      received-after-date
                      (legacy-date received-after-date))]
     (future
       (->> m
            (find-in-inbox [:received-after after-date])
            (filter #(let [addr (or (-> % :from first :address) "unknown")]
                       (includes? addr "bankofamerica"))))))))

(defn extract-body [msg] 
  (let [{:keys [body]} msg]
    (condp #(starts-with? %2 %1) (or (:content-type body) "")
      "TEXT/HTML" (html->text (:body body))
      "TEXT/PLAIN" (:body body)
      "")))

(defn body-text [mail-msg]
  (-> mail-msg
      extract-body
      (.replaceAll "\\R" " ")
      #_(.replaceAll "\\$" "")
      (.replaceAll "\\$ " "")))

(defn find-amount
  "TODO - add docs here..."
  [words sign]
  (when-let [amt-index ((some-fn #(string/index-of % "Amount:")
                                 #(string/index-of % "Amount")) words)]
    (let [val-begin (string/index-of words \space amt-index)
          val-end    (string/index-of words \space (inc val-begin))
          actual-end (or val-end (.length words))]
      #_(tap> {:value-amt (subs words val-begin actual-end)})
      (-> (subs words val-begin actual-end)
          string/trim
          (string/replace #"\$" "")
          (string/replace #"," "")
          bigdec
          (* sign)))))

(defn find-date
  "TODO - add docs here..."
  [words]
  (when-let [date-index ((some-fn #(string/index-of % "January")
                                  #(string/index-of % "February")
                                  #(string/index-of % "March")
                                  #(string/index-of % "April")
                                  #(string/index-of % "May")
                                  #(string/index-of % "June")
                                  #(string/index-of % "July")
                                  #(string/index-of % "August")
                                  #(string/index-of % "September")
                                  #(string/index-of % "October")
                                  #(string/index-of % "November")
                                  #(string/index-of % "December")) words)]
    (->> (take 3
               (-> (subs words date-index)
                   string/trim
                   (string/split #" ")))
         (string/join " "))))

(defn find-merchant
  "TODO - add docs here..."
  [words]
  (when-let [merch-index ((some-fn #(string/index-of % "made at")
                                   #(string/index-of % "Made at")
                                   #(string/index-of % "Where:")
                                   #(string/index-of % "From:")
                                   #(string/index-of % "Type")) words)]
    (subs words
          (+ 4 merch-index)
          (min (+ 50 4 merch-index) (.length words)))))

(defn parse-body [mail-body]
  (let [words (body-text mail-body)]
    {:on (find-date words)
     :amt (find-amount words (if (string/includes? words "credited") 1 -1))
     :merchant (find-merchant words)}))

  (defn when-done
    "Invoke a fn when a future completes. Returns a future wrapping the result
   of the fn to call."
    [future-to-watch fn-to-call]
    (future (fn-to-call @future-to-watch)))

;; TODO - I think the map here is only processing the first txn
;; Need to catch exception if parse-body throws
;; Location of conf-service needs to be configurable (cmd-line arg?)
  (defn extract-values-from-txns []
    (when-done
     (recent-boa (fetch-account
                  "http://localhost:8080/v1/config/account/gmail-tstout")
                 #_"02-Jul-2024")
     (fn [fut-result] (map parse-body fut-result))))

  (defn poller [minutes]
    (periodic-fn (* 1000 60 minutes)
                 (fn []
                   (log/info "Excecuting email poll...")
                   (let [parsings @(extract-values-from-txns)]
                     (log/infof "Found %d transactions...inserting" (count parsings))
                     (doseq [txn (filter map? parsings)]
                       (if (every? nil? (vals txn)) 
                         (log/info "Email encountered without any amount...expected")
                         (insert-checking txn)))))))

  (def email-poller
    (delay (poller 5)
           (log/info "Email poller started - 5 minute interval")))

  (comment
    *e
    (add-tap println)
    (remove-tap println)

    (def recent (recent-boa (fetch-account
                             "http://localhost:8080/v1/config/account/gmail-tstout")
                            "20-Jun-2025"))

    (realized? recent)

    (def email (-> "/Users/tstout/src/fin-data/email.edn"
                   slurp
                   clojure.edn/read-string))
    email

    (parse-body email)
    
    (nth @recent 2)
    (count @recent)

    (first @recent)
    
    (count @recent)

    (body-text (nth @recent 23))

  ;; Exercise new parsing impls
    (-> (body-text (nth @recent 28))
        (find-amount -1))

    (-> (body-text (nth @recent 28))
        find-date)

    (-> (body-text (nth @recent 28))
        find-merchant)

    (extract-body (nth @recent 28))
    
    (:content-type (:body (nth @recent 32)))
    (:content-type (nth @recent 32)) 

    (parse-body (nth @recent 6)) 
    
    (body-text (nth @recent 24)) 
    
    (def parsings (extract-values-from-txns))
    (nth @parsings 1)

    (realized? parsings)
    (count @parsings)
    parsings

    (first @parsings)

    (legacy-date "02-Jul-2024")

    ;;
    )
