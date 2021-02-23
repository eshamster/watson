(defpackage :watson/definer/import
  (:use #:cl)
  (:export #:defimport.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-import
                #:make-wat-import
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|import|
                #:|func|
                #:func
                #:|memory|
                #:memory)
  (:import-from #:watson/parser/type
                #:parse-typeuse)
  (:import-from #:watson/parser/misc
                #:parse-var-name
                #:parse-mod-nm)
  (:import-from #:cl-ppcre
                #:split))
(in-package :watson/definer/import)

;; https://webassembly.github.io/spec/core/text/modules.html#imports

(defmacro defimport.wat (name mod-nm import-desc)
  ;; Ex. (defimport.wat lg console.log (func ((i32))))
  ;;     -> (import "console" "log" (func $lg (param i32)))
  `(setf (wsymbol-import (intern.wat ',name))
         (generate-import-body
          ',name ',mod-nm ',import-desc)))

(defun generate-import-body (name mod-nm parsed-import-desc)
  (multiple-value-bind (import-desc types)
      (parse-import-desc name parsed-import-desc)
    (make-wat-import
     :generator (lambda ()
                  `(|import|
                    ,@(parse-mod-nm mod-nm)
                    ,import-desc))
     :arg-types types)))

(defun parse-import-desc (name import-desc)
  ;; TODO: should process 'table'
  (let ((keyword (car import-desc))
        (params (cdr import-desc)))
    (ecase keyword
      (func (parse-import-func-desc name params))
      (memory (parse-import-memory-desc name params)))))

(defun parse-import-func-desc (name params)
  ;; Ex. name: foo, params: (((a i32) (b i32)) (i32))
  ;;     -> (func $foo (param $a i32) (param $b i32) (result $i32))
  (multiple-value-bind (typeuse variables types)
      (parse-typeuse params)
    (declare (ignore variables))
    (values `(|func| ,(parse-var-name name)
                     ,@typeuse)
            types)))

(defun parse-import-memory-desc (name params)
  ;; Ex. name: foo, params: (1)
  ;;     -> (memory $foo 1)
  `(|memory| ,(parse-var-name name)
             ,(car params)))
