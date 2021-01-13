(defpackage :watson/definer/global
  (:use #:cl)
  (:export #:defglobal.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-global
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|global|
                #:global
                #:|mut|
                #:mut
                #:|import|)
  (:import-from #:watson/parser/type
                #:convert-type)
  (:import-from #:watson/parser/misc
                #:parse-var-name
                #:parse-mod-nm))
(in-package :watson/definer/global)

(defmacro defglobal.wat (name mod-nm globaltype)
  ;; Ex. (defglobal.wat g js.global (mut i32))
  ;;     -> (global $g (import "js" "global") (mut i32))
  `(progn (setf (wsymbol-global (intern.wat ',name))
                (lambda ()
                  (generate-global-body
                   ',name ',mod-nm ',globaltype)))))

(defun generate-global-body (name mod-nm globaltype)
  `(|global|
    ,(parse-var-name name)
    (|import| ,@(parse-mod-nm mod-nm))
    ,(parse-globaltype globaltype)))

(defun parse-globaltype (globaltype)
  (cond ((and (listp globaltype)
              (eq (car globaltype) 'mut))
         `(|mut| ,(convert-type (cadr globaltype))))
        (t (error "not implemented globaltype form: ~A" globaltype))))
