(defpackage :watson/define/import
  (:use #:cl)
  (:export #:defimport.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-import
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
(in-package :watson/define/import)

;; https://webassembly.github.io/spec/core/text/modules.html#imports

(defmacro defimport.wat (name mod-nm import-desc)
  ;; Ex. (defimport.wat lg console.log (func ((i32))))
  ;;     -> (import "console" "log" (func $lg (param i32)))
  `(progn (setf (wsymbol-import (intern.wat ',name))
                (lambda ()
                  (generate-import-body
                   ',name ',mod-nm ',import-desc)))))

(defun generate-import-body (name mod-nm parsed-import-desc)
  `(|import|
    ,@(parse-mod-nm mod-nm)
    ,(parse-import-desc name parsed-import-desc)))

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
  `(|func| ,(parse-var-name name)
           ,@(parse-typeuse params)))

(defun parse-import-memory-desc (name params)
  ;; Ex. name: foo, params: (1)
  ;;     -> (memory $foo 1)
  `(|memory| ,(parse-var-name name)
             ,(car params)))
