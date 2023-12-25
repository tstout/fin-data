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
                   26.19M                                amt)
          (parse-body (load-res :type-6))))

(defexpect parse-type-3
  (expect (more-of {:keys [merchant amt on type]}
                   :type-3                               type
                   "at TST* J MACKLINS GRILL--COPPELL ,TX Transaction type: PURCH W/O PIN" merchant
                   "December 16, 2023"                   on
                   107.96M                               amt)
          (parse-body (load-res :type-3))))

;; {:on "December 16, 2023",
;;  :on-index 33,
;;  :at-index 21,
;;  :amt 107.96M,
;;  :merchant "at TST* J MACKLINS GRILL--COPPELL ,TX Transaction type: PURCH W/O PIN",
;;  :type :type-3}

(comment
  *e
  (run-tests)
  (parse-body (load-res :type-6))

  (locate-words-of-interest
   (load-res :type-3)
   #{"Amount:" "at:" "When:"})

;;
  )