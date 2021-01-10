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
        #:watson/env/type
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

           ;; type
           #:i32
           #:i64
           #:f32
           #:f64

           ;; default macros
           #:for

           #:i32+
           #:i32-
           #:i32*
           #:i32s/
           #:i32u/

           #:i64+
           #:i64-
           #:i64*
           #:i64s/
           #:i64u/

           #:f32+
           #:f32-
           #:f32*
           #:f32/

           #:f64+
           #:f64-
           #:f64*
           #:f64/

           ;; reserved words
           #:get-local
           #:set-local
           #:get-global
           #:set-global

           #:br
           #:br-if

           ;; built-in functions
           #:i32.load
           #:i32.load8-s
           #:i32.load8-u
           #:i32.load16-s
           #:i32.load16-u
           #:i32.store
           #:i32.store8
           #:i32.store16

           #:i64.load
           #:i64.load8-s
           #:i64.load8-u
           #:i64.load16-s
           #:i64.load16-u
           #:i64.load32-s
           #:i64.load32-u
           #:i64.store
           #:i64.store8
           #:i64.store16
           #:i64.store32

           #:f32.load
           #:f32.store

           #:f64.load
           #:f64.store

           #:memory.size
           #:memory.grow

           #:i32.const
           #:i64.const
           #:f32.const
           #:f64.const

           #:i32.clz
           #:i32.ctz
           #:i32.popcnt
           #:i32.add
           #:i32.sub
           #:i32.mul
           #:i32.div-s
           #:i32.div-u
           #:i32.rem-s
           #:i32.rem-u
           #:i32.and
           #:i32.or
           #:i32.xor
           #:i32.shl
           #:i32.shr-s
           #:i32.shr-u
           #:i32.rotl
           #:i32.rotr

           #:i64.clz
           #:i64.ctz
           #:i64.popcnt
           #:i64.add
           #:i64.sub
           #:i64.mul
           #:i64.div-s
           #:i64.div-u
           #:i64.rem-s
           #:i64.rem-u
           #:i64.and
           #:i64.or
           #:i64.xor
           #:i64.shl
           #:i64.shr-s
           #:i64.shr-u
           #:i64.rotl
           #:i64.rotr

           #:f32.abs
           #:f32.neg
           #:f32.ceil
           #:f32.floor
           #:f32.trunc
           #:f32.nearest
           #:f32.sqrt
           #:f32.add
           #:f32.sub
           #:f32.mul
           #:f32.div
           #:f32.min
           #:f32.max
           #:f32.copysign

           #:f64.abs
           #:f64.neg
           #:f64.ceil
           #:f64.floor
           #:f64.trunc
           #:f64.nearest
           #:f64.sqrt
           #:f64.add
           #:f64.sub
           #:f64.mul
           #:f64.div
           #:f64.min
           #:f64.max
           #:f64.copysign

           #:i32.eqz
           #:i32.eq
           #:i32.ne
           #:i32.lt-s
           #:i32.lt-u
           #:i32.gt-s
           #:i32.gt-u
           #:i32.le-s
           #:i32.le-u
           #:i32.ge-s
           #:i32.ge-u

           #:i64.eqz
           #:i64.eq
           #:i64.ne
           #:i64.lt-s
           #:i64.lt-u
           #:i64.gt-s
           #:i64.gt-u
           #:i64.le-s
           #:i64.le-u
           #:i64.ge-s
           #:i64.ge-u

           #:f32.eq
           #:f32.ne
           #:f32.lt
           #:f32.gt
           #:f32.le
           #:f32.ge

           #:f64.eq
           #:f64.ne
           #:f64.lt
           #:f64.gt
           #:f64.le
           #:f64.ge

           #:i32.wrap-i64
           #:i32.trunc-f32-s
           #:i32.trunc-f32-u
           #:i32.trunc-f64-s
           #:i32.trunc-f64-u
           #:i32.trunc-sat-f32-s
           #:i32.trunc-sat-f32-u
           #:i32.trunc-sat-f64-s
           #:i32.trunc-sat-f64-u

           #:i64.extend-i32-s
           #:i64.extend-i32-u
           #:i64.trunc-f32-s
           #:i64.trunc-f32-u
           #:i64.trunc-f64-s
           #:i64.trunc-f64-u
           #:i64.trunc-sat-f32-s
           #:i64.trunc-sat-f32-u
           #:i64.trunc-sat-f64-s
           #:i64.trunc-sat-f64-u

           #:f32.convert-i32-s
           #:f32.convert-i32-u
           #:f32.convert-i64-s
           #:f32.convert-i64-u
           #:f32.demote-f64

           #:f64.convert-i32-s
           #:f64.convert-i32-u
           #:f64.convert-i64-s
           #:f64.convert-i64-u
           #:f64.promote-f64
           #:f32.reinterpret-f32
           #:f64.reinterpret-f64
           #:f32.reinterpret-i32
           #:f64.reinterpret-i64

           #:i32.extend8-s
           #:i32.extend16-s
           #:i64.extend8-s
           #:i64.extend16-s
           #:i64.extend32-s))
