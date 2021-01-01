(defpackage :watson/define/global
  (:use #:cl)
  (:export #:defglobal.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|global|
                #:global
                #:|mut|
                #:mut)
  (:import-from #:watson/parser/type
                #:convert-type)
  (:import-from #:watson/parser/misc
                #:parse-arg-name
                #:parse-mod-nm))
(in-package :watson/define/global)

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
