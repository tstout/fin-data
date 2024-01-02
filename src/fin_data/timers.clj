(ns fin-data.timers
  (:require [clojure.core.async :refer [go-loop chan alt! timeout put!]]))

(defn- run
  [msecs f]
  (let [stop-ch (chan)]
    (go-loop []
      (alt!
        (timeout msecs) (do (f)
                            (recur))
        stop-ch nil))
    stop-ch))

(defn run-periodically [msecs f]
  {:pre [(fn? f) (int? msecs)]}
  (let
   [stop-ch   (atom (run msecs f))
    timer-ops {:start (fn [] (reset! stop-ch (run msecs f)))
               :stop  (fn [] (put! @stop-ch true))}]
    (fn [operation & args] (-> (timer-ops operation) (apply args)))))

(comment
  (def tmr (run-periodically 5000 #(println "Fn executed!")))
  (tmr :stop)
  (tmr :start)

;;
  )