(ns fin-data.core-test
  (:require [clojure.test :refer [use-fixtures run-tests]]
            [expectations.clojure.test :refer [defexpect
                                               expect expecting]]))

(defn setup [f] (f))

(use-fixtures :once setup)

(defexpect placeholder (expect 1 1))

(comment
  *e
  (run-tests)

;;(remove-ns 'fin-data.core-test)

  "see https://github.com/clojure-expectations/clojure-test for examples")
