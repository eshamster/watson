(defpackage :watson/define/export
  (:use #:cl)
  (:export #:defexport.wat
           #:get-export-body-generators)
  (:import-from #:watson/reserved-word
                #:|export|
                #:|func|
                #:func)
  (:import-from #:watson/utils
                #:parse-arg-name
                #:symbol-to-string)
  (:import-from #:alexandria
                #:hash-table-values))
(in-package :watson/define/export)

;; https://webassembly.github.io/spec/core/text/modules.html#exports

(defvar *exports* (make-hash-table))

(defun get-export-body-generators ()
  (hash-table-values *exports*))

(defmacro defexport.wat (js-func-name export-desc)
  ;; Ex. (defexport.wat js-func-name (func foo))
  ;;     -> (export "js_func_name" (func $foo))
  `(setf (gethash ',js-func-name *exports*)
         (lambda ()
           (generate-export-body ',js-func-name ',export-desc))))

(defun generate-export-body (js-func-name export-desc)
  `(|export|
    ,(symbol-to-string js-func-name)
    ,(parse-export-desc export-desc)))

(defun parse-export-desc (export-desc)
  (let ((type  (car  export-desc))
        (param (cadr export-desc)))
    (ecase type
      (func (parse-export-func-desc param)))))

(defun parse-export-func-desc (param)
  `(|func| ,(parse-arg-name param)))
