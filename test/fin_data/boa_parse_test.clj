(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body body-text]]
            [expectations.clojure.test :refer [defexpect
                                               expect
                                               expecting
                                               more-of]]))

(defexpect parse-type-6
  (expect (more-of {:keys [merchant amt on]} 
                   " at: DD *DOORDASH CAVA -SAN FRANCISCO,CA On: December 16, 2023 View transaction details This transac" merchant
                   "December 16, 2023"                   on
                   -26.19M                                amt)
          (parse-body (load-res :type-6))))

(defexpect parse-type-7
  (expect (more-of {:keys [merchant amt on]} 
                   ": ACME PAYROLL EXT DIR DEP View deposit details Create your Life Plan Bank of America Life Plan® is " merchant
                   "December 21, 2023"                   on
                   -1691.33M                             amt)
          (parse-body (load-res :type-7))))

(defexpect parse-type-3
  (expect (more-of {:keys [merchant amt on]} 
                   "e: at TST* J MACKLINS GRILL--COPPELL ,TX Transaction type: PURCH W/O PIN When: on December 16, 2023 " merchant
                   "December 16, 2023"                   on
                   -107.96M                              amt)
          (parse-body (load-res :type-3))))

(defexpect parse-type-4
  (expect (more-of {:keys [merchant amt on]} 
                   ": ELEC DRAFT (ACH) Account: PERSONAL CHECKING/SAVINGS ACCOUNT - 1234 Merchant: NEW YORK LIFE INS. PR" merchant
                   "December 15, 2023"                    on
                   -226.64M                               amt)
          (parse-body (load-res :type-4))))

(comment
  *e
  (run-tests)
  (parse-type-7)
  (parse-body (load-res :type-7)) 

  (body-text (load-res :type-7))
  (load-res :type-3)

;;
  )