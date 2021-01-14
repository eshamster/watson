(defpackage :watson/definer/export
  (:use #:cl)
  (:export #:defexport.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-export
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|export|
                #:|func|
                #:func)
  (:import-from #:watson/parser/misc
                #:parse-var-name)
  (:import-from #:watson/util/symbol
                #:symbol-to-string)
  (:import-from #:alexandria
                #:hash-table-keys))
(in-package :watson/definer/export)

;; https://webassembly.github.io/spec/core/text/modules.html#exports

(defmacro defexport.wat (js-func-name export-desc)
  ;; Ex. (defexport.wat js-func-name (func foo))
  ;;     -> (export "js_func_name" (func $foo))
  `(setf (wsymbol-export (intern.wat ',(get-export-target export-desc)))
         (lambda ()
           (generate-export-body ',js-func-name ',export-desc))))

(defun get-export-target (export-desc)
  (cadr export-desc))

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
  `(|func| ,(parse-var-name param)))
