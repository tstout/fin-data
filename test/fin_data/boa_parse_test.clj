(ns fin-data.boa-parse-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [fin-data.fixtures :refer [load-res]]
            [fin-data.boa-parse :refer [parse-body parse-transactions body-text]]))

(deftest parse-type-6
  (is (= {:merchant "TEST MERCHANT TYPE 6"
          :on       "January 4, 2000"
          :amt      -33.33M}
         (parse-body (load-res :type-6)))))

(deftest parse-type-7
  (is (= {:merchant "TEST MERCHANT TYPE 7"
          :on       "January 5, 2000"
          :amt      44.44M}
         (parse-body (load-res :type-7)))))

(deftest parse-type-3
  (is (= {:merchant "TEST MERCHANT TYPE 3"
          :on       "January 2, 2000"
          :amt      -11.11M}
         (parse-body (load-res :type-3)))))

(deftest parse-type-4
  (is (= {:merchant "TEST MERCHANT TYPE 4"
          :on       "January 3, 2000"
          :amt      -22.22M}
         (parse-body (load-res :type-4)))))

;;   

(deftest fix-nil-merchant
  (is (= {:merchant "TEST MERCHANT"
          :on       "January 1, 2000"
          :amt      -1.23M}
         (parse-body (load-res :nil-merchant)))))

(deftest parse-multiple-transactions-in-html-email
  (is (= [{:amt -12.34M
           :merchant "TEST MERCHANT ONE"
           :on "January 2, 2000"}
          {:amt -56.78M
           :merchant "TEST MERCHANT TWO"
           :on "January 3, 2000"}]
         (parse-transactions (load-res :multi-txn)))))

(deftest parse-multiple-transfers-in-html-email
  (let [transactions (parse-transactions (load-res :type-12))]
    (is (= 2 (count transactions)))
    (is (= 2 (count (set (map :amt transactions)))))
    (is (every? (fn [{:keys [amt merchant on]}]
                  (and (number? amt)
                       (string? merchant)
                       (string? on)))
                transactions))))

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