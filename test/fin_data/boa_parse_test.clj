(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body body-text]]))

(deftest parse-type-6
  (is (= {:merchant "DD *DOORDASH CAVA -SAN FRANCISCO,CA"
          :on       "December 16, 2023"
          :amt      -26.19M}
         (parse-body (load-res :type-6)))))

(deftest parse-type-7
  (is (= {:merchant "ACME PAYROLL EXT DIR DEP"
          :on       "December 21, 2023"
          :amt      -1691.33M}
         (parse-body (load-res :type-7)))))

(deftest parse-type-3
  (is (= {:merchant "at TST* J MACKLINS GRILL--COPPELL ,TX"
          :on       "December 16, 2023"
          :amt      -107.96M}
         (parse-body (load-res :type-3)))))

(deftest parse-type-4
  (is (= {:merchant "NEW YORK LIFE INS. PREM."
          :on       "December 15, 2023"
          :amt      -226.64M}
         (parse-body (load-res :type-4)))))

(deftest parse-html-merchant-label-aliases
  (doseq [label ["Made at" "Where:" "From" "Type" "Merchant"]]
    (let [html (str "<table><tr><td>" label
                    "</td><td> ACME PAYROLL </td></tr></table>")
          email {:body {:content-type "TEXT/HTML"
                        :body html}}]
      (is (= "ACME PAYROLL"
             (:merchant (parse-body email)))
          (str "merchant label: " label)))))

(comment
  *e
  (run-tests)
  (parse-type-7)
  (parse-body (load-res :type-7)) 

  (body-text (load-res :type-7))
  (load-res :type-3)

;;
  )