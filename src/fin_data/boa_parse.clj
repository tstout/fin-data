(ns fin-data.boa-parse
  (:require [clojure-mail.core :refer [search-inbox]]
            [clojure-mail.gmail :as gmail]
            [clojure-mail.message :as message]
            [clojure-mail.parser :refer [html->text]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string :refer [includes? starts-with?]]
            [fin-data.creds :refer [fetch-account]]))

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
  "expensive operation, returns a future so that interaction in a repl won't get 
   blocked for a long period of time"
  [m]
  (future
    (->> m
         (find-in-inbox [:received-after :yesterday])
         (filter #(let [addr (-> % :from first :address)]
                    (includes? addr "bankofamerica"))))))

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
  (-> mail-msg
      extract-body
      (string/split #" ")
      vec))

(defn locate-values-of-interest
  "Given a vector of words extracted from an email body, determine the 
   index within that vector of key elements within the email. These
   include the amount, date, and location of purchase. A map of 
   {:words [<vector of strings>] 
    :values (([index \"Amount:\"] [index \"on:\"] [index \"at:\"])...)}
   
   See also extract-values-of-interest.
   As of 05-Nov-2023, the BOA email body text contains these items of interest:
   Amount:
   On:
   at:
   "
  [mail-body]
  (let [words (body-words mail-body)]
    {:words  words
     :values (keep-indexed (fn [index item]
                             (when (#{"Amount:" "On:" "at:"} item)
                               [index item]))
                           words)}))

(defn extract-amt [index words]
  (-> (subvec words (+ 1 index) (+ 3 index))
      last
      bigdec))

;; TODO - this is going to vary. Need to compute the end of the 
;; merchant via the index difference of On: and at:
;; Obvious option is to reduce on the coll of coordinate vectors
(defn extract-merchant [txn words]
  (let [{:keys [at-index on-index]} txn
        merchant (string/join " " (subvec words (+ 1 at-index) on-index))]
    (merge txn {:merchant merchant})))

(defn extract-date [index words]
  (string/join " " (subvec words (+ 1 index) (+ 4 index))))

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
                    "Amount:" (merge {:amt (extract-amt index words)} accum)
                    "at:"     (merge {:at-index index} accum)
                    "On:"     (merge {:on       (extract-date index words)
                                      :on-index index}     accum))))
              {}
              txn)
      (extract-merchant words)))

(defn extract-values-of-interest [mail-body]
  (let [{:keys [words values]} (locate-values-of-interest mail-body)
        txns                   (partition-all 3 values)]
    #_(prn txns)
    (map #(process-txn %1 words) txns)))

(defn extract-values-from-txns [mail-bodies]
  (let [recent (recent-boa
                (fetch-account
                 "http://localhost:8080/v1/config/account/gmail-tstout"))]
    ))


(comment
  (def recent (recent-boa
               (fetch-account
                "http://localhost:8080/v1/config/account/gmail-tstout")))

  (def t-words (-> (first @recent)
                   locate-values-of-interest
                   :words))

  t-words

  (subvec t-words 24 29)

  (locate-values-of-interest (first @recent))

  (pprint (extract-values-of-interest (first @recent)))

  (extract-values-of-interest (first @recent))

  (first @recent)

  ;;(mod )

  (realized? recent)

  (count @recent)

  (type (first @recent))

  (-> @recent first :body)

  @recent

  ;;
  )
