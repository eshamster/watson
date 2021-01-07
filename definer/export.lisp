(defpackage :watson/definer/export
  (:use #:cl)
  (:export #:defexport.wat
           #:get-export-body-generators)
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

(defvar *exports* (make-hash-table))

(defun get-export-body-generators (package)
  (mapcar (lambda (sym)
            (gethash sym *exports*))
          (remove-if (lambda (sym)
                       (not (eq package (symbol-package sym))))
                     (hash-table-keys *exports*))))

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
  `(|func| ,(parse-var-name param)))
