(defpackage :watson/defun
  (:use #:cl
        #:watson/reserved-word)
  (:export #:defun.wat)
  (:import-from #:watson/body-parser
                #:parse-body)
  (:import-from #:watson/environment
                #:wsymbol-function
                #:intern.wat
                #:wenv-function-symbols
                #:wenv-import-symbols)
  (:import-from #:watson/type
                #:convert-type
                #:parse-typeuse)
  (:import-from #:watson/utils
                #:parse-arg-name)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/defun)

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
      ,(parse-arg-name name)
      ,@parsed-typeuse
      ,@(parse-body body vars))))

(defun defun-empty% (name args)
  (let ((args-var (mapcar #'car args)))
    `(defun ,name ,args-var
       (declare (ignore ,@args-var)))))
