(defpackage :watson/t/definer/export
  (:use #:cl
        #:rove
        #:watson/definer/export)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-export
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|export|
                #:func
                #:|func|))
(in-package :watson/t/definer/export)

(deftest defexport.wat
  (let ((i32.add 'watson/env/built-in-func::|i32.add|))
    (let ((tests `((:name "simple case"
                    :init ,(lambda ()
                             (defexport.wat js-func (func hoge)))
                    :target-sym hoge
                    :expect (|export| "js_func" (|func| $hoge))))))
      (dolist (tt tests)
        (destructuring-bind (&key name init target-sym expect) tt
          (with-cloned-wenvironment
            (testing name
              (funcall init)
              (ok (equalp (funcall (wsymbol-export (intern.wat target-sym)))
                          expect)))))))))
