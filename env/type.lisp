(defpackage :watson/env/type
  (:use #:cl)
  (:export #:convert-type

           #:i32
           #:|i32|
           #:i64
           #:|i64|
           #:f32
           #:|f32|
           #:f64
           #:|f64|)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print))
(in-package :watson/env/type)

(defvar *type-table* (make-hash-table))

(defun convert-type (type-sym)
  (multiple-value-bind (got found)
      (gethash type-sym *type-table*)
    (unless found
      (error "~A is not valid type" type-sym))
    got))

(defmacro def-wat-type (sym)
  `(progn (defvar ,sym nil)
          (setf (gethash ',sym *type-table*)
                ',(sym-to-sym-for-print sym))))

;; Cf. https://webassembly.github.io/spec/core/text/types.html

(def-wat-type i32)
(def-wat-type i64)
(def-wat-type f32)
(def-wat-type f64)
