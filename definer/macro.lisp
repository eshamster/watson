(defpackage :watson/definer/macro
  (:use #:cl)
  (:export #:defmacro.wat)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms)
  (:import-from #:watson/env/environment
                #:wsymbol-macro-function
                #:intern.wat))
(in-package :watson/definer/macro)

;; TODO: distinguish between default macros and user defined macros

(defmacro defmacro.wat (name lambda-list &body body)
  `(progn (defmacro.wat% ,name ,lambda-list ,@body)
          ;; NOTE: Also define with %-suffix as CL macro to mainly try expanding in editor.
          (defmacro ,(symbolicate name "%") ,lambda-list ,@body)))

(defmacro defmacro.wat% (name lambda-list &body body)
  (with-gensyms (params env)
    `(setf (wsymbol-macro-function (intern.wat ',name))
           (lambda (,params ,env)
             (declare (ignorable ,env))
             (destructuring-bind ,lambda-list (cdr ,params)
               ,@body)))))
