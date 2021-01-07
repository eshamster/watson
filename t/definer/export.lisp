(defpackage :watson/t/definer/export
  (:use #:cl
        #:rove
        #:watson/definer/export)
  (:import-from #:watson/env/reserved-word
                #:|export|
                #:func
                #:|func|))
(in-package :watson/t/definer/export)

(defmacro with-test-exports ((var)  &body body)
  `(let* ((watson/definer/export::*exports* (make-hash-table))
          (,var watson/definer/export::*exports*))
     ,@body))

(deftest defexport.wat
  (let ((tests `((:name "export function"
                  :init ,(lambda ()
                           (defexport.wat js-func (func hoge)))
                  :js-func-name js-func
                  :expect (|export| "js_func" (|func| $hoge))))))
    (dolist (tt tests)
      (destructuring-bind (&key name init js-func-name expect) tt
        (with-test-exports (exports)
          (testing name
            (funcall init)
            (ok (equalp (funcall (gethash js-func-name exports))
                        expect))
            (ok (= (length (get-export-body-generators *package*))
                   1))))))))
