(defpackage :watson/define/function
  (:use #:cl
        #:watson/env/reserved-word)
  (:export #:defun.wat)
  (:import-from #:watson/parser/body-parser
                #:parse-body)
  (:import-from #:watson/env/environment
                #:wsymbol-function
                #:intern.wat
                #:wenv-function-symbols
                #:wenv-import-symbols)
  (:import-from #:watson/parser/type
                #:convert-type
                #:parse-typeuse)
  (:import-from #:watson/parser/misc
                #:parse-var-name)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/define/function)

(defmacro defun.wat (name args result &body body)
  `(progn (setf (wsymbol-function (intern.wat ',name))
                (lambda ()
                  (generate-defun ',name ',args ',result ',body)))
          ,(unless (eq (symbol-package name)
                       (find-package "CL"))
             (defun-empty% name args))))

(defun generate-defun (name args result body)
  (multiple-value-bind (parsed-typeuse vars)
      (parse-typeuse (list args result))
    `(|func|
      ,(parse-var-name name)
      ,@parsed-typeuse
      ,@(parse-body body vars))))

(defun defun-empty% (name args)
  (let ((args-var (mapcar #'car args)))
    `(defun ,name ,args-var
       (declare (ignore ,@args-var)))))
