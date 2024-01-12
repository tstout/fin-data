(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [use-fixtures run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body locate-words-of-interest]]
            [expectations.clojure.test :refer [defexpect
                                               expect
                                               expecting
                                               more-of]]))

(defexpect parse-type-6
  (expect (more-of {:keys [merchant amt on type]}
                   :type-6                               type
                   "DD *DOORDASH CAVA -SAN FRANCISCO,CA" merchant
                   "December 16, 2023"                   on
                   -26.19M                                amt)
          (parse-body (load-res :type-6))))

(defexpect parse-type-7
  (expect (more-of {:keys [merchant amt on type]}
                   :type-7                               type
                   "ACME PAYROLL EXT DIR DEP View"       merchant
                   "December 21, 2023"                   on
                   1691.33M                              amt)
          (parse-body (load-res :type-7))))

(defexpect parse-type-3
  (expect (more-of {:keys [merchant amt on type]}
                   :type-3                               type
                   "at TST* J MACKLINS GRILL--COPPELL ,TX Transaction type: PURCH W/O PIN" merchant
                   "December 16, 2023"                   on
                   -107.96M                               amt)
          (parse-body (load-res :type-3))))

(defexpect parse-type-4
  (expect (more-of {:keys [merchant amt on type]}
                   :type-4                                type
                   "NEW YORK LIFE INS. PREM. Transaction" merchant
                   "December 15, 2023"                    on
                   -226.64M                                amt)
          (parse-body (load-res :type-4))))

(comment
  *e
  (run-tests)
  (parse-body (load-res :type-6))

  (locate-words-of-interest
   (load-res :type-3)
   #{"Amount:" "at:" "When:"})

;;
  )