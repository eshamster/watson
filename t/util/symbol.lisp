(defpackage :watson/t/util/symbol
  (:use #:cl
        #:rove
        #:watson/util/symbol))
(in-package :watson/t/util/symbol)

(deftest symbol-to-string
  (let ((tests '((:name "simple case"
                  :input test
                  :expect "test")
                 (:name "characters are converted to downcase"
                  :input |TeSt|
                  :expect "test")
                 (:name "hyphen is converted to underbar"
                  :input t-e_s-t
                  :expect "t_e_s_t"))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect) tt
        (testing name
          (ok (string= (symbol-to-string input)
                       expect)))))))

(deftest sym-to-sym-for-print
  (let ((tests '((:name "simple case"
                  :input test
                  :expect |test|)
                 (:name "characters are converted to downcase"
                  :input |TeSt|
                  :expect |test|)
                 (:name "hyphen is converted to underbar"
                  :input t-e_s-t
                  :expect |t_e_s_t|))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect) tt
        (testing name
          (ok (eq (sym-to-sym-for-print input)
                  expect)))))))
