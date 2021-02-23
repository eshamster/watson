(defpackage :watson/t/env/const-func
  (:use #:cl
        #:rove
        #:watson/env/const-func)
  (:import-from #:watson/env/type
                #:i32))
(in-package :watson/t/env/const-func)

(deftest const-func-p
  (let ((tests '((:name "const func"
                  :input i32.const
                  :expect t)
                 (:name "not const func"
                  :input hoge
                  :expect nil))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect) tt
        (testing name
          (let ((result (const-func-p input)))
            (if expect
                (ok result)
                (ok (not result)))))))))

(deftest convert-const-func
  (let ((tests '((:name "normal case"
                  :input i32.const
                  :expect watson/env/const-func::|i32.const|)
                 (:name "ERROR: not const function"
                  :input hoge
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (convert-const-func input)))
              (ok (eq (convert-const-func input)
                      expect))))))))

(deftest convert-type-to-const-func
  (let ((tests '((:name "normal case"
                  :input i32
                  :expect watson/env/const-func::|i32.const|)
                 (:name "ERROR: not type"
                  :input hoge
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (convert-type-to-const-func input)))
              (ok (eq (convert-type-to-const-func input)
                      expect))))))))
