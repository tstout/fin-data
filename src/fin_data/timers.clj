(ns fin-data.timers
  (:require [clojure.core.async :refer [go-loop chan alt! timeout put!]]
            [clojure.tools.logging :as log]))

;; based this on https://stackoverflow.com/a/53559455/59768
;; Stop does not actually kill the go block however

;; TODO - considering adding ability to run at a specific
;; time of day (desired date/time - current data/time in ms)
;; For now, this feature is not necessary.
;; Also, consider adding some stats: last invocation exception,
;; invocation count, exception count
(defn- run
  [msecs f]
  (let [stop-ch (chan)]
    (go-loop []
      (alt!
        (timeout msecs) (do
                          (try
                            (f)
                            (catch Exception e
                              (log/error e "periodic-fn exception")))
                          (recur))
        stop-ch nil))
    stop-ch))

(defn periodic-fn [msecs f]
  {:pre [(fn? f) (int? msecs)]}
  (let
   [stop-ch   (atom (run msecs f))
    timer-ops {:start (fn [] (reset! stop-ch (run msecs f)))
               :stop  (fn [] (put! @stop-ch true))}]
    (fn [operation & args] (-> (timer-ops operation) (apply args)))))

(comment
  (def tmr (periodic-fn 5000 #(println "Fn executed!")))
  (def tmr-ex (periodic-fn 5000 #(throw (Exception. "Fn executed!"))))

  (tmr-ex :stop)
  (tmr :stop)
  (tmr :start)

;;
  )