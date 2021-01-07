(defpackage :watson/main
  (:nicknames #:watson)
  (:use #:watson/env/built-in-func
        #:watson/definer/global
        #:watson/definer/macro
        #:watson/definer/function
        #:watson/definer/export
        #:watson/definer/import
        #:watson/module
        #:watson/parser/macro
        #:watson/env/reserved-word
        #:watson/default/macro)
  (:export #:defmacro.wat
           #:defun.wat
           #:defexport.wat
           #:defimport.wat
           #:defglobal.wat
           #:generate-wat-module

           #:func
           #:memory
           #:mut
           #:local
           #:block
           #:loop

           ;; default macros
           #:for
           #:i32+
           #:i32-

           ;; built-in functions
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
           #:i32.load

           #:get-local
           #:set-local
           #:get-global
           #:set-global

           #:br
           #:br-if))
