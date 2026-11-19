(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body body-text]]))

(deftest parse-type-6
  (is (= {:merchant " at: DD *DOORDASH CAVA -SAN FRANCISCO,CA On: December 16, 2023 View transaction details This transac"
          :on "December 16, 2023"
          :amt -26.19M}
         (parse-body (load-res :type-6)))))

(deftest parse-type-7
  (is (= {:merchant ": ACME PAYROLL EXT DIR DEP View deposit details Create your Life Plan Bank of America Life Plan® is "
          :on "December 21, 2023"
          :amt -1691.33M}
         (parse-body (load-res :type-7)))))

(deftest parse-type-3
  (is (= {:merchant "e: at TST* J MACKLINS GRILL--COPPELL ,TX Transaction type: PURCH W/O PIN When: on December 16, 2023 "
          :on "December 16, 2023"
          :amt -107.96M}
         (parse-body (load-res :type-3)))))

(deftest parse-type-4
  (is (= {:merchant ": ELEC DRAFT (ACH) Account: PERSONAL CHECKING/SAVINGS ACCOUNT - 1234 Merchant: NEW YORK LIFE INS. PR"
          :on "December 15, 2023"
          :amt -226.64M}
         (parse-body (load-res :type-4)))))

(comment
  *e
  (run-tests)
  (parse-type-7)
  (parse-body (load-res :type-7)) 

  (body-text (load-res :type-7))
  (load-res :type-3)

;;
  )