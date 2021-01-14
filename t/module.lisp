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
  (:import-from #:watson/definer/global
                #:defglobal.wat)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment)
  (:import-from #:watson/env/reserved-word
                #:func
                #:mut)
  (:import-from #:watson/env/type
                #:i32))
(in-package :watson/t/module)

(deftest generate-wat-module
  (testing "count elements"
    (with-cloned-wenvironment
      ;; define
      (defun.wat fn1 () ())
      (defun.wat fn2 () ())
      (defun.wat fn3 () ())
      (defimport.wat import1 a.b (func ()))
      (defimport.wat import2 c.d (func ()))
      (defimport.wat import3 c.d (func ()))
      (defimport.wat import4 e.f (func ()))
      (defglobal.wat global1 js.global (mut i32))
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
          (ok (= (count-target "global") 1))
          (ok (= (count-target "export") 2))))))
  (with-cloned-wenvironment
    ;; define
    (defun.wat fn () ())
    (defimport.wat import a.b (func ()))
    (defglobal.wat g js.global (mut i32))
    (defexport.wat js-func-name (func fn))
    ;; test
    (let ((*print-right-margin* 1000))
      (ok (string= (format nil "~A" (generate-wat-module *package*))
                   "(module (import \"a\" \"b\" (func $IMPORT)) (global $G (import \"js\" \"global\") (mut i32)) (func $FN) (export \"js_func_name\" (func $FN)))")))))
