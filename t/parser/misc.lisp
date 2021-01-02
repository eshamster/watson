(defpackage :watson/t/parser/misc
  (:use #:cl
        #:rove
        #:watson/parser/misc))
(in-package :watson/t/parser/misc)

(deftest parse-var-name
  (let ((tests '((:name "normal case"
                  :input test
                  :expect $test))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect) tt
        (testing name
          (ok (eq (parse-var-name input)
                  expect)))))))

(deftest parse-mod-nm
  (let ((tests '((:name "normal case"
                  :input test.abc
                  :expect ("test" "abc"))
                 (:name "ERROR: illegal format"
                  :input test
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (parse-mod-nm input)))
              (ok (equalp (parse-mod-nm input)
                          expect))))))))
