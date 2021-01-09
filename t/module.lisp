(defpackage :watson/t/module
  (:use #:cl
        #:rove
        #:watson/module)
  (:import-from #:watson/definer/function
                #:defun.wat)
  (:import-from #:watson/definer/import
                #:defimport.wat)
  (:import-from #:watson/definer/export
                #:defexport.wat)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment)
  (:import-from #:watson/env/reserved-word
                #:func))
(in-package :watson/t/module)

(defmacro with-test-env (&body body)
  `(let ((watson/definer/export::*exports* (make-hash-table)))
     (with-cloned-wenvironment
       ,@body)))

(deftest generate-wat-module
  (testing "count elements"
    (with-test-env
      ;; define
      (defun.wat fn1 () ())
      (defun.wat fn2 () ())
      (defun.wat fn3 () ())
      (defimport.wat import1 a.b (func ()))
      (defimport.wat import2 c.d (func ()))
      (defimport.wat import3 c.d (func ()))
      (defimport.wat import4 e.f (func ()))
      (defexport.wat js-func-name1 (func fn1))
      (defexport.wat js-func-name2 (func fn2))
      ;; test
      (let ((result (generate-wat-module *package*)))
        (flet ((count-target (target)
                 (length (remove-if (lambda (exp)
                                      (not (string= (car exp) target)))
                                    (cdr result)))))
          (ok (= (count-target "func") 3))
          (ok (= (count-target "import") 4))
          (ok (= (count-target "export") 2))))))
  (with-test-env
    ;; define
    (defun.wat fn () ())
    (defimport.wat import a.b (func ()))
    (defexport.wat js-func-name (func fn))
    ;; test
    (let ((*print-right-margin* 1000))
      (ok (string= (format nil "~A" (generate-wat-module *package*))
                   "(module (import \"a\" \"b\" (func $IMPORT)) (func $FN) (export \"js_func_name\" (func $FN)))")))))
