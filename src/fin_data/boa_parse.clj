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

;; TODO do some nested map destructuring here
(defn extract-body [msg]
  (let [{:keys [body]} msg]
    (condp #(starts-with? %2 %1) (:content-type body)
      "TEXT/HTML" (html->text (:body body))
      "TEXT/PLAIN" (:body body))))

(defn body-words
  "Create a vector of the words comprising the body of the email."
  [mail-msg]
  (let [words (-> mail-msg
                  extract-body
                  (.replaceAll "\\R" " ")
                  (string/split #" "))]
    (-> (map #(string/trim %) words)
        vec)))

(defn extract-merchant [txn words]
  (let [{:keys [at-index on-index]} txn
        merchant (string/join " " (subvec words (+ 1 at-index) on-index))]
    (merge txn {:merchant merchant})))

(defn extract-depositor [txn words]
  ;; This is a bit of a punt at the moment.
  ;; The pattern of parsing for merchant/depositor breaks down
  ;; here. There is nothing in the text to indicate the end of the
  ;; depositor description. This merely truncates after so many words.
  ;; Ugly, but likely no problem for my purposes.
  (let [{:keys [at-index]} txn
        merchant (string/join
                  " "
                  (subvec
                   words
                   (+ 1 at-index)
                   (min (+ 7 at-index) (count words))))]
    (merge txn {:merchant merchant})))

(defn locate-words-of-interest
  "Given a vector of words extracted from an email body and a set of words of interest,
   determine the index within that vector of key elements within the email. 
   Returns a map of:
     {:words [<vector of strings>] 
      :values ([index \"Amount:\"] [index \"on:\"] [index \"at:\"])...)}
     "
  [mail-body key-set]
  {:pre [(set? key-set)]}
  (let [words (body-words mail-body)]
    {:words  words
     :values (keep-indexed (fn [index item]
                             (when (key-set item)
                               [index item]))
                           words)}))

(defn parse-dispatch [mail-body]
  (->> mail-body
       body-words
       (filter #(string/ends-with? % ":"))
       distinct
       vec))

(defn extract-amt
  ([index words sign]
   (-> (subvec words (+ 1 index) (+ 3 index))
       last
       (string/replace #"," "")
       bigdec
       (* sign)))
  ([index words]
   (extract-amt index words -1)))

(defn extract-date [index words]
  (string/join " " (subvec words (+ 1 index) (+ 4 index))))

;; Note: use of #' (var) for dispatch fn to support REPL reloading.
(defmulti parse-body #'parse-dispatch)

(defmethod parse-body ["Amount:" "at:" "On:"] [mail-body]
  (log/error "received type-1 mail, need to implement")
  [:type-1])

(defmethod parse-body ["with:" "immediately:"] [_]
  (log/info "Ignoring [with: immediately:]")
  [:type-13])

(defmethod parse-body ["2024:" "#:" "Amount:" "To:" "Fee:" "Service:" "policy:" "Lender:" "to:"] [_]
  (log/info "Ignoring :type-14")
  [:type-14])

(defmethod parse-body ["check:" "Amount:" "number:" "date:"] [mail-body]
  (log/error "received type-2 mail, need to implemen`t")
  [:type-2])

(defmethod parse-body ["Amount:" "card:" "Where:" "type:" "When:"] [mail-body]
  (let [{:keys [words values]} (locate-words-of-interest
                                mail-body
                                #{"Amount:" "Where:" "When:"})]
    (-> (reduce (fn [accum coordinate]
                  (let [[index pos-key] coordinate]
                    (case pos-key
                      "Amount:"  (merge {:amt (extract-amt index words)} accum)
                      "Where:"   (merge {:at-index index} accum)
                      "When:"    (merge {:on       (string/join " " (subvec words (+ 2 index) (+ 5 index)))
                                         :on-index index}     accum))))
                {}
                values)
        (extract-merchant words)
        (merge {:type :type-3}))))

(defmethod parse-body ["Amount:" "Type:" "Account:" "Merchant:" "date:"] [mail-body]
  (let [{:keys [words values]} (locate-words-of-interest
                                mail-body
                                #{"Amount:" "Merchant:" "date:"})]
    (-> (reduce (fn [accum coordinate]
                  (let [[index pos-key] coordinate]
                    (case pos-key
                      "Amount:"   (merge {:amt (extract-amt index words)} accum)
                      "Merchant:" (merge {:at-index index} accum)
                      "date:"     (merge {:on       (extract-date index words)
                                          :on-index index}     accum))))
                {}
                values)
        (extract-merchant words)
        (merge {:type :type-4}))))

(defmethod parse-body ["Account:" "Amount:" "at:" "On:" "Account:" "Amount:" "at:" "On:"] [mail-body]
  ;; Same as type-6?
  [:type-5])

(defmethod parse-body ["Account:" "Amount:" "at:" "On:"] [mail-body]
  ;; TODO - words is not a good sym name here
  (let [{:keys [words values]} (locate-words-of-interest
                                mail-body
                                #{"Amount:" "at:" "On:"})]
    (-> (reduce (fn [accum coordinate]
                  (let [[index pos-key] coordinate]
                    #_(prn (format "index: %d pos-key: %s" index pos-key))
                    (case pos-key
                      "Amount:"  (merge {:amt (extract-amt index words)} accum)
                      "at:"      (merge {:at-index index} accum)
                      "On:"      (merge {:on       (extract-date index words)
                                         :on-index index}     accum))))
                {}
                values)
        (extract-merchant words)
        (merge {:type :type-6}))))

(defmethod parse-body ["Amount:" "Account:" "On:" "From:"] [mail-body]
  (let [{:keys [words values]} (locate-words-of-interest
                                mail-body
                                #{"Amount:" "From:" "On:"})]
    (-> (reduce (fn [accum coordinate]
                  (let [[index pos-key] coordinate]
                    (case pos-key
                      "Amount:"  (merge {:amt (extract-amt index words 1)} accum)
                      "From:"    (merge {:at-index index} accum)
                      "On:"      (merge {:on       (extract-date index words)
                                         :on-index index}     accum))))
                {}
                values)
        (extract-depositor words)
        (merge {:type :type-7}))))

(defmethod parse-body ["transfer:" "Amount:" "date:"] [mail-body]
  (log/error "received type-10 mail, need to implement")
  [:type-10])

(defmethod parse-body [] [mail-body]
  [:ignore])

(defmethod parse-body ["amount:" "To:" "on:" "now:" "#:" "information:"] [mail-body]
  (log/error "received type-8 mail, need to implement")
  [:type-8])

(defmethod parse-body ["Account:"] [_mail-body]
  ;; Statement available email - probably will just ignore this.
  [:type-9])

(defmethod parse-body ["days:" "Item:" "Amount:" "Date:" "To:" "Fee:" "Service:" "policy:" "Lender:" "to:"] [mail-body]
  ;; Online transfer reminder..ignore?
  [:type-11])

(defmethod parse-body ["Amount:" "Account:" "On:" "From:" "number:"] [_mail-body]
  ;; Deposit - multiple transactions indicated by number
  ;; Need to deal with multiple txns here
  (log/info "received type-12 mail, need to implement")
  [:type-12])

;;["Amount:" "Account:" "On:" "From:" "number:"]

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
               "02-Jul-2024")
   (fn [fut-result] (map parse-body fut-result))))

(defn poller [minutes]
  (periodic-fn (* 1000 60 minutes)
               (fn []
                 (log/info "Excecuting email poll...")
                 (let [parsings @(extract-values-from-txns)]
                   (log/infof "Found %d transactions...inserting" (count parsings))
                   (doseq [txn (filter map? parsings)]
                     (insert-checking txn))))))

(def email-poller
  (delay (poller 10)
         (log/info "Email poller started - 10 minute interval")))

(defn dump-words
  "Dump the word vector from an email into a file named
   mail-words.txt for analysis. See also extract-keys."
  [mail key-set]
  {:pre [(set? key-set)]}
  (let [words (-> mail
                  (locate-words-of-interest key-set)
                  :words)]
    (doseq [w words]
      (spit "mail-words.txt" (str w \newline) :append true))))

(defn dump-mail-body [mail-body]
  (spit "email.edn" (with-out-str
                      (pprint mail-body))))

(comment
  *e

  (def recent (recent-boa (fetch-account
                           "http://localhost:8080/v1/config/account/gmail-tstout")
                          "02-Jul-2024"))

  (realized? recent)

  (nth @recent 2)
  (count @recent)

  (dump-mail-body (nth @recent 3))

  (def poll (poller 2))
  (poll :start)
  (poll :stop)

  (require 'user)
  (user/trace! #'extract-merchant)
  (user/untrace! #'extract-merchant)

  (def parsings (extract-values-from-txns))

  (realized? parsings)
  (count @parsings)
  parsings

  (first @parsings)

  (legacy-date "02-Jul-2024")

  ;; This is what you need to filter in some cases?
  (not (Character/isDigit (first "a23:")))

;;
  )
