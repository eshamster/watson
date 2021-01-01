(defpackage :watson/define/macro
  (:use #:cl)
  (:export #:defmacro.wat)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms)
  (:import-from #:watson/environment
                #:wsymbol-macro-function
                #:intern.wat))
(in-package :watson/define/macro)

;; TODO: distinguish between default macros and user defined macros

(defmacro defmacro.wat (name lambda-list &body body)
  (with-gensyms (params env)
    `(progn (setf (wsymbol-macro-function (intern.wat ',name))
                  (lambda (,params ,env)
                    (declare (ignorable ,env))
                    (destructuring-bind ,lambda-list (cdr ,params)
                      ,@body)))
            ;; NOTE: Also define with %-suffix as CL macro to mainly try expanding in editor.
            (defmacro ,(symbolicate name "%") ,lambda-list ,@body))))
