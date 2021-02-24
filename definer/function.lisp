(defpackage :watson/definer/function
  (:use #:cl
        #:watson/env/reserved-word)
  (:export #:defun.wat)
  (:import-from #:watson/parser/body-parser
                #:parse-body)
  (:import-from #:watson/env/environment
                #:wsymbol-function
                #:make-wat-function
                #:intern.wat)
  (:import-from #:watson/parser/type
                #:convert-type
                #:parse-typeuse)
  (:import-from #:watson/parser/misc
                #:parse-var-name)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/definer/function)

(defmacro defun.wat (name args result &body body)
  `(progn (setf (wsymbol-function (intern.wat ',name))
                (generate-defun ',name ',args ',result ',body))
          ,(unless (eq (symbol-package name)
                       (find-package "CL"))
             (defun-empty% name args))))

(defun generate-defun (name args result body)
  (multiple-value-bind (parsed-typeuse vars types)
      (parse-typeuse (list args result))
    (make-wat-function
     :generator (lambda ()
                  `(|func|
                    ,(parse-var-name name)
                    ,@parsed-typeuse
                    ,@(parse-body body vars types)))
     :arg-types types)))

(defun defun-empty% (name args)
  (let ((args-var (mapcar #'car args)))
    `(defun ,name ,args-var
       (declare (ignore ,@args-var)))))
