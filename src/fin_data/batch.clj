(ns fin-data.batch
  (:require [fin-data.db-io :refer [purge-logs]]
            [clojure.tools.logging :as log]
            [fin-data.boa-parse :as boa]
            [fin-data.timers :refer [daily-fn]]))
           
(def log-cleaner
  (delay (daily-fn "America/Chicago" 19 00 purge-logs)
         (log/info "Log cleaner started - every day at 19:00 CST")))

(def email-poller
  (delay (boa/poller 5)
         (log/info "Email poller started - 5 minute interval")))

(defn start-batch-jobs []
  @log-cleaner 
  @email-poller 
  (log/info "Batch jobs started"))


(comment
  ;;
  )