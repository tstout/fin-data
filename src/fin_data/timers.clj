(ns fin-data.timers
  "Timer functionality for executing a function every x milliseconds
   or at a specific time of day."
  (:require [clojure.core.async :refer [go-loop chan alt! timeout close! thread]]
            [clojure.tools.logging :as log])
  (:import [java.time Duration LocalTime ZoneId ZonedDateTime]))

;; Also, consider adding some stats: last invocation exception,
;; invocation count, exception count
(defn- run
  [delay-fn f]
  (let [stop-ch (chan)]
    (go-loop []
      (alt!
        (timeout (delay-fn)) (do
                               (thread
                                 (try
                                   (f)
                                   (catch Exception e
                                     (log/error e "periodic-fn exception"))))
                               (recur))
        stop-ch nil))
    stop-ch))

(defn- timer-fn [delay-fn f]
  (let
   [stop-ch   (atom (run delay-fn f))
    timer-ops {:start (fn []
                        (close! @stop-ch)
                        (reset! stop-ch (run delay-fn f)))
               :stop  (fn [] (close! @stop-ch))}]
    (fn [operation & args] (-> (timer-ops operation) (apply args)))))

(defn periodic-fn [msecs f]
  {:pre [(fn? f) (int? msecs)]}
  (timer-fn (constantly msecs) f))

(defn- delay-until [zone hour minute]
  (let [now      (ZonedDateTime/now zone)
        target   (LocalTime/of hour minute)
        today    (-> (.toLocalDate now)
                     (.atTime target)
                     (.atZone zone))
        next-run (if (.isAfter today now)
                   today
                   (-> (.toLocalDate now)
                       (.plusDays 1)
                       (.atTime target)
                       (.atZone zone)))]
    (.toMillis (Duration/between now next-run))))

(defn daily-fn [zone-id hour minute f]
  {:pre [(string? zone-id)
         (int? hour)
         (int? minute)
         (fn? f)]}
  (let [zone (ZoneId/of zone-id)]
    (timer-fn #(delay-until zone hour minute) f)))

(comment
  (def tmr (periodic-fn 5000 #(println "Fn executed!")))
  (def tmr-ex (periodic-fn 5000 #(throw (Exception. "Fn executed!"))))

  (def tmr-daily (daily-fn "America/Chicago" 18 21 #(println "Daily Fn executed!")))

  (tmr-daily :start)
  (tmr-daily :stop)

  (tmr-ex :stop)
  (tmr-ex :start)
  (tmr :stop)
  (tmr :start)

;;
  )