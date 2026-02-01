(ns fin-data.creds
  (:require [clojure.edn :as edn]
            [conceal.core :refer [reveal mk-opts key-from-env]]
            [clojure.tools.logging :as log])
  (:import [java.net.http
            HttpClient
            HttpRequest
            HttpResponse$BodyHandlers]
           [java.net URI]))

(defn decrypt-txt [txt]
  (-> txt
      (mk-opts (key-from-env))
      reveal))

(defn decrypt-creds [creds]
  (let [{:keys [user_id pass]} creds]
    (merge creds {:user (decrypt-txt user_id)
                  :pass (decrypt-txt pass)})))

(defn get-request [uri]
  (-> (HttpRequest/newBuilder)
      .GET
      (.uri (URI/create uri))
      (.setHeader "User-Agent" "Java 11+")
      .build))

(defn http-tx
  "Transmit an http request."
  [req]
  (-> (HttpClient/newHttpClient)
      (.send req (HttpResponse$BodyHandlers/ofString))))

;; TODO - put try catch rethrow here
(defn fetch-account [uri]
  (log/infof "fetch-acccount %s" uri)
  (try 
    (-> uri
        get-request
        http-tx
        .body
        edn/read-string
        first
        decrypt-creds)
    (catch Exception e
      (log/error "fetch-account error" e)
      (throw (ex-info "Error fetching account creds"
                      {:uri uri}
                      e)))))

(comment

  (fetch-account
   "http://stout-pi4:8080/v1/config/account/gmail-tstout")

  ;;
  )