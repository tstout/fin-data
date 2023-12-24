(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [use-fixtures run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body]]
            [expectations.clojure.test :refer [defexpect
                                               expect
                                               expecting
                                               more-of]]))

(defexpect parse-type-6
  (expect (more-of {:keys [merchant amt on type]}
                   :type-6                               type
                   "DD *DOORDASH CAVA -SAN FRANCISCO,CA" merchant
                   "December 16, 2023"                   on
                   26.19M                                amt)
          (parse-body (load-res :type-6))))

(comment
  *e
  (run-tests)
  (parse-body (load-res :type-6))
  ;;
  )