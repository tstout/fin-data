(ns fin-data.fixtures
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn load-res [res-name]
  {:pre [(keyword? res-name)]}
  (->
   res-name
   name
   (str ".edn")
   io/resource
   slurp
   edn/read-string))

(comment

  (load-res :type-6)


  ;;
  )