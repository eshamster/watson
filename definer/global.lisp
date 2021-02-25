(defpackage :watson/definer/global
  (:use #:cl)
  (:export #:defglobal.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-global
                #:make-wat-global
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
                (generate-global-body
                 ',name ',mod-nm ',globaltype))))

(defun generate-global-body (name mod-nm globaltype)
  (multiple-value-bind (parsed-globaltype type)
      (parse-globaltype globaltype)
    (make-wat-global
     :generator (lambda ()
                  `(|global|
                    ,(parse-var-name name)
                    (|import| ,@(parse-mod-nm mod-nm))
                    ,parsed-globaltype))
     :type type)))

(defun parse-globaltype (globaltype)
  (cond ((and (listp globaltype)
              (eq (car globaltype) 'mut))
         (let ((type (cadr globaltype)))
           (values `(|mut| ,(convert-type type))
                   type)))
        (t (error "not implemented globaltype form: ~A" globaltype))))
