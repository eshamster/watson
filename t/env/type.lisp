(defpackage :watson/t/env/type
  (:use #:cl
        #:rove
        #:watson/env/type))
(in-package :watson/t/env/type)

(deftest convert-type
  (let ((tests '((:name "type"
                  :input i32
                  :expect |i32|)
                 (:name "ERROR: not type"
                  :input hoge
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (convert-type input)))
              (ok (eq (convert-type input) expect))))))))
