(ns fin-data.boa-parse
  (:require [clojure-mail.core :refer [search-inbox]]
            [clojure-mail.gmail :as gmail]
            [clojure.java.io :as io]
            [clojure-mail.message :as message]
            [clojure-mail.parser :refer [html->text]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string :refer [includes? starts-with?]]
            [fin-data.creds :refer [fetch-account]])
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
   period of time"
  ([m]
   (recent-boa m :yesterday))
  ([m received-after-date]
   (let [after-date (if (keyword? received-after-date)
                      received-after-date
                      (legacy-date received-after-date))]
     (future
       (->> m
            (find-in-inbox [:received-after after-date])
            (filter #(let [addr (-> % :from first :address)]
                       (includes? addr "bankofamerica"))))))))

(defn boa-alerts [m]
  (->> m
       (find-in-inbox "Bank Of America")
       (map message/read-message)
       vec))

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

(defn extract-amt [index words]
  (-> (subvec words (+ 1 index) (+ 3 index))
      last
      bigdec))

(defn extract-date [index words]
  (string/join " " (subvec words (+ 1 index) (+ 4 index))))

;; Note: use of #' (var) for dispatch fn to support REPL reloading.
(defmulti parse-body #'parse-dispatch)

(defmethod parse-body ["Amount:" "at:" "On:"] [mail-body]
  [:type-1])

(defmethod parse-body ["check:" "Amount:" "number:" "date:"] [mail-body]
  [:type-2])

(defmethod parse-body ["Amount:" "card:" "Where:" "type:" "When:"] [mail-body]
  [:type-3])

(defmethod parse-body ["Amount:" "Type:" "Account:" "Merchant:" "date:"] [mail-body]
  [:type-4])

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
  [:type-7])

(defmethod parse-body ["transfer:" "Amount:" "date:"] [mail-body]
  [:type-7])

(defmethod parse-body [] [mail-body]
  [:ignore])

(defmethod parse-body ["amount:" "To:" "on:" "now:" "#:" "information:"] [mail-body]
  [:type-8])

;; (defmethod parse-body ["Amount:" "card:" "Where:" "type:" "When:"] [mail-body]
;;   [:type-4])

;; TODO - this is going to vary. Need to compute the end of the 
;; merchant via the index difference of On: and at:
;; Obvious option is to reduce on the coll of coordinate vectors

(defn- process-txn
  "Reduce on this txn structure ([index pos-key] ...):
  ([19 \"Amount:\"] [24 \"at:\"] [28 \"On:\"]) 
  words is a collection of the words making up the mail body"
  [txn words]
  ;;(prn txn)
  ;; TODO - consider using the case to populate indices needed
  ;; for merchant range calc and add a final map operation to add
  ;; The merchant def...seems clean...implement this.
  (-> (reduce (fn [accum coordinate]
                (let [[index pos-key] coordinate]
                  #_(prn (format "index: %d pos-key: %s" index pos-key))
                  (case pos-key
                    "Amount:"  (merge {:amt (extract-amt index words)} accum)
                    "at:"      (merge {:at-index index} accum)
                    "Merchant" (merge {:at-index index} accum)
                    "On:"      (merge {:on       (extract-date index words)
                                       :on-index index}     accum)
                    "date:"    (merge {:on       (extract-date index words)
                                       :on-index index}     accum))))
              {}
              txn)
      (extract-merchant words)))

(defn extract-values-of-interest [mail-body key-set]
  (let [{:keys [words values]} (locate-words-of-interest mail-body key-set)
        txns                   (partition-all 3 values)]
    #_(prn txns)
    (map #(process-txn %1 words) txns)))

(defn when-done
  "Invoke a fn when a future completes. Returns a future wrapping the result
   of the fn to call."
  [future-to-watch fn-to-call]
  (future (fn-to-call @future-to-watch)))

;; TODO - I think the map here is only processing the first txn
(defn extract-values-from-txns []
  (when-done
   (recent-boa (fetch-account "http://localhost:8080/v1/config/account/gmail-tstout"))
   #(map extract-values-of-interest %)))

;; Use extract-keys and dump-words to look at the shape of the 
;; email words. 
(defn extract-keys
  "It is expected that the format of emails from BOA will change over time.
   Use this fn in a REPL to view the possible key strings from the vector of email body
   words stored in a resource file."
  [email-words]
  (->> email-words
       io/resource
       slurp
       string/split-lines
       (filter #(string/ends-with? % ":"))))

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

(comment
  *e
  (legacy-date "12-DEC-1990")

  (.parse (SimpleDateFormat. "dd-MMM-yyyy") "05-May-1970")

  (def recent (recent-boa
               (fetch-account
                "http://localhost:8080/v1/config/account/gmail-tstout")))

  (realized? recent)

  (count @recent)

    ;;(extract-body (nth @recent 2))

    ;;(nth @recent 3)

  (dump-words (nth @recent 5) {})

  (parse-body (nth @recent 1))

  (def t-txn (parse-body (nth @recent 3)))

  t-txn

  (extract-values-of-interest (nth @recent 3) #{"Amount:" "at:" "On:"})

  (second @recent)

  ;; This is what you need to filter in some cases
  (not (Character/isDigit (first "a23:")))

  (vec (extract-keys "type-5.txt"))
  (= #{1 2 3} #{3 2 1})

  ;; Parsing body words can result in this:
  (frequencies
   ["Account:" "Amount:" "at:" "On:"
    "Account:" "Amount:" "at:" "On:"
    "Account:" "Amount:" "at:" "On:"])

  ;; distinct can find the pattern needed for defmethod
  ;; parse methods should assume their might be more than one
  ;; txn
  (distinct
   ["Account:" "Amount:" "at:" "On:"
    "Account:" "Amount:" "at:" "On:"
    "Account:" "Amount:" "at:" "On:"])

;;
  )
