(defpackage :watson/t/definer/function
  (:use #:cl
        #:rove
        #:watson/definer/function)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-function
                #:wenv-function-body-generators
                #:intern.wat)
  (:import-from #:watson/env/built-in-func
                #:i32.add)
  (:import-from #:watson/env/reserved-word
                #:|func|
                #:|param|
                #:|result|
                #:|get_local|)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|))
(in-package :watson/t/definer/function)

(deftest defun.wat
  (let ((i32.add 'watson/env/built-in-func::|i32.add|))
    (let ((tests `((:name "simple case"
                    :init ,(lambda ()
                             (defun.wat hoge ((a i32) (b i32)) (i32)
                               (i32.add a b)))
                    :expect (|func| $hoge
                                    (|param| $a |i32|) (|param| $b |i32|)
                                    (|result| |i32|)
                                    (,i32.add (|get_local| $a) (|get_local| $b)))))))
      (dolist (tt tests)
        (destructuring-bind (&key name init expect) tt
          (with-cloned-wenvironment
            (testing name
              (funcall init)
              (let ((funcs (wenv-function-body-generators *package*)))
                (ok (= (length funcs) 1))
                (ok (equalp (funcall (car funcs))
                            expect))))))))))

