(defpackage :watson/t/env/built-in-func
  (:use #:cl
        #:rove
        #:watson/env/built-in-func)
  (:import-from :watson/env/type
                #:i32))
(in-package :watson/t/env/built-in-func)

(deftest built-in-func-p
  (let ((tests '((:name "built-in func"
                  :input i32.eq
                  :expect t)
                 (:name "not built-in func"
                  :input hoge
                  :expect nil))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect) tt
        (testing name
          (let ((result (built-in-func-p input)))
            (if expect
                (ok result)
                (ok (not result)))))))))

(deftest convert-built-in-func
  (let ((tests '((:name "normal case"
                  :input i32.rem-u
                  :expect watson/env/built-in-func::|i32.rem_u|)
                 (:name "ERROR: not built-in function"
                  :input hoge
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (convert-built-in-func input)))
              (ok (eq (convert-built-in-func input)
                      expect))))))))

(deftest get-arg-types-for-built-in-func
  (let ((tests '((:name "type-operator"
                  :input (:sym i32.add
                          :args (a b))
                  :expect (i32 i32))
                 (:name "built-in-func but not type-operator"
                  :input (:sym memory.size
                          :args (a b))
                  :expect nil)
                 (:name "ERROR: not built-in-func"
                  :input (:sym hoge
                          :args (a b))
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (destructuring-bind (&key sym args) input
            (if expect-err
                (ok (signals (get-arg-types-for-built-in-func sym args)))
                (ok (equalp (get-arg-types-for-built-in-func sym args)
                            expect)))))))))
