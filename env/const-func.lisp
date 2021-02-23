(defpackage :watson/env/const-func
  (:use #:cl)
  (:export #:const-func-p
           #:convert-const-func
           #:convert-type-to-const-func

           #:i32.const
           #:i64.const
           #:f32.const
           #:f64.const)
  (:import-from #:watson/env/type
                #:i32
                #:i64
                #:f32
                #:f64)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/env/const-func)

(defvar *const-funcs* (make-hash-table))
(defvar *const-funcs-by-type* (make-hash-table))

(defun const-func-p (sym)
  (gethash sym *const-funcs*))

(defun convert-const-func (sym)
  (multiple-value-bind (result ok)
      (const-func-p sym)
    (assert ok nil "~A is not a const func" sym)
    result))

(defun convert-type-to-const-func (type)
  (multiple-value-bind (result ok)
      (gethash type *const-funcs-by-type*)
    (assert ok nil "type ~A has not const-func" type)
    result))

(defmacro def-const-func (type)
  (let ((sym (symbolicate type ".CONST")))
    `(progn (defvar ,sym nil)
            (setf (gethash ',sym *const-funcs*)
                  ',(sym-to-sym-for-print sym))
            (setf (gethash ',type *const-funcs-by-type*)
                  ',(sym-to-sym-for-print sym)))))

(def-const-func i32)
(def-const-func i64)
(def-const-func f32)
(def-const-func f64)
