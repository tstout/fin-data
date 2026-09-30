(ns fin-data.batch
  (:require [fin-data.db-io :refer [purge-logs]]
            [clojure.tools.logging :as log]
            [fin-data.timers :refer [daily-fn]]))

(def log-cleaner
  (delay (daily-fn "America/Chicago" 19 00 purge-logs)
         (log/info "Log cleaner started - every day at 19:00 CST")))