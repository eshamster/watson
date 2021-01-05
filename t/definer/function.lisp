(defpackage :watson/t/definer/function
  (:use #:cl
        #:rove
        #:watson/definer/function)
  (:import-from #:watson/env/environment
                #:*global-wat-env*
                #:clone-wenvironment
                #:wsymbol-function
                #:intern.wat)
  (:import-from #:watson/env/built-in-func
                #:i32.add)
  (:import-from #:watson/env/reserved-word
                #:i32
                #:|i32|
                #:|func|
                #:|param|
                #:|result|
                #:|get_local|))
(in-package :watson/t/definer/function)

(deftest defun.wat
  (let ((i32.add 'watson/env/built-in-func::|i32.add|))
    (let ((tests `((:name "simple case"
                    :init ,(lambda ()
                             (defun.wat hoge ((a i32) (b i32)) (i32)
                               (i32.add a b)))
                    :target-sym hoge
                    :expect (|func| $hoge
                                    (|param| $a |i32|) (|param| $b |i32|)
                                    (|result| |i32|)
                                    (,i32.add (|get_local| $a) (|get_local| $b)))))))
      (dolist (tt tests)
        (destructuring-bind (&key name init target-sym expect) tt
          (let ((*global-wat-env* (clone-wenvironment)))
            (testing name
              (funcall init)
              (ok (equalp (funcall (wsymbol-function (intern.wat target-sym)))
                          expect)))))))))

