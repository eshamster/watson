(defpackage :watson/defglobal
  (:use #:cl)
  (:export #:defglobal.wat)
  (:import-from #:watson/environment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:watson/reserved-word
                #:|global|
                #:global
                #:|mut|
                #:mut)
  (:import-from #:watson/type
                #:convert-type)
  (:import-from #:watson/utils
                #:parse-arg-name
                #:parse-mod-nm))
(in-package :watson/defglobal)

(defmacro defglobal.wat (name mod-nm globaltype)
  ;; Ex. (defglobal.wat g js.global (mut i32))
  ;;     -> (global $g (import "js" "global") (mut i32))
  `(progn (setf (wsymbol-import (intern.wat ',name))
                (lambda ()
                  (generate-global-body
                   ',name ',mod-nm ',globaltype)))))

(defun generate-global-body (name mod-nm globaltype)
  `(|global|
    ,(parse-arg-name name)
    (|import| ,@(parse-mod-nm mod-nm))
    ,(parse-globaltype globaltype)))

(defun parse-globaltype (globaltype)
  (cond ((and (listp globaltype)
              (eq (car globaltype) 'mut))
         `(|mut| ,(convert-type (cadr globaltype))))
        (t (error "not implemented globaltype form: ~A" globaltype))))
