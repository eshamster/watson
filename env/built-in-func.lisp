(defpackage :watson/env/built-in-func
  (:use #:cl)
  (:export #:built-in-func-p
           #:convert-built-in-func

           #:i32.add
           #:i32.sub
           #:i32.mul
           #:i32.rem-u
           #:i32.const
           #:i32.eq
           #:i32.eqz
           #:i32.ge-u
           #:i32.gt-u
           #:i32.store
           #:i32.load)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print)
  (:import-from #:cl-ppcre
                #:regex-replace-all))
(in-package :watson/env/built-in-func)

(defvar *built-in-funcs* (make-hash-table))

(defun built-in-func-p (sym)
  (gethash sym *built-in-funcs*))

(defun convert-built-in-func (sym)
  (assert (built-in-func-p sym))
  (gethash sym *built-in-funcs*))

(defmacro def-built-in-func (sym &optional sym-for-print)
  `(progn (defvar ,sym nil)
          (setf (gethash ',sym *built-in-funcs*)
                ',(if sym-for-print
                      sym-for-print
                      (sym-to-sym-for-print sym)))))

(def-built-in-func i32.add)
(def-built-in-func i32.sub)
(def-built-in-func i32.mul)
(def-built-in-func i32.rem-u)
(def-built-in-func i32.const)
(def-built-in-func i32.eq)
(def-built-in-func i32.eqz)
(def-built-in-func i32.ge-u)
(def-built-in-func i32.gt-u)
(def-built-in-func i32.store)
(def-built-in-func i32.load)

(def-built-in-func get-global |global.get|)
(def-built-in-func set-global |global.set|)
