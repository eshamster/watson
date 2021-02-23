(defpackage :watson/env/built-in-func
  (:use #:cl)
  (:export #:built-in-func-p
           #:convert-built-in-func

           ;; - memory instructions - ;;

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

           ;; - numeric instructions - ;;

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
           #:i64.extend32-s)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/env/built-in-func)

(defvar *built-in-funcs* (make-hash-table))

(defun built-in-func-p (sym)
  (gethash sym *built-in-funcs*))

(defun convert-built-in-func (sym)
  (assert (built-in-func-p sym) nil
          "~A is not a build-in-func" sym)
  (gethash sym *built-in-funcs*))

(defmacro def-built-in-func (sym &optional sym-for-print)
  `(progn (defvar ,sym nil)
          (setf (gethash ',sym *built-in-funcs*)
                ',(if sym-for-print
                      sym-for-print
                      (sym-to-sym-for-print sym)))))

(defmacro def-type-operators (type operations)
  `(progn ,@(mapcar (lambda (op)
                      `(def-built-in-func
                           ,(symbolicate type "." op)))
                    operations)))

;; - Memory Instructions - ;;
;; Cf. https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions

(def-type-operators i32
    (load
     load8-s
     load8-u
     load16-s
     load16-u
     store
     store8
     store16))

(def-type-operators i64
    (load
     load8-s
     load8-u
     load16-s
     load16-u
     load32-s
     load32-u
     store
     store8
     store16
     store32))

(def-type-operators f32
    (load
     store))

(def-type-operators f64
    (load
     store))

(def-built-in-func memory.size)
(def-built-in-func memory.grow)

;; - Numeric Instructions - ;;
;; Cf. https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions

;; - basic - ;;

(def-type-operators i32
    (clz
     ctz
     popcnt
     add
     sub
     mul
     div-s
     div-u
     rem-s
     rem-u
     and
     or
     xor
     shl
     shr-s
     shr-u
     rotl
     rotr))

(def-type-operators i64
    (clz
     ctz
     popcnt
     add
     sub
     mul
     div-s
     div-u
     rem-s
     rem-u
     and
     or
     xor
     shl
     shr-s
     shr-u
     rotl
     rotr))

(def-type-operators f32
    (abs
     neg
     ceil
     floor
     trunc
     nearest
     sqrt
     add
     sub
     mul
     div
     min
     max
     copysign))

(def-type-operators f64
    (abs
     neg
     ceil
     floor
     trunc
     nearest
     sqrt
     add
     sub
     mul
     div
     min
     max
     copysign))

;; - comparative operators - ;;

(def-type-operators i32
    (eqz
     eq
     ne
     lt-s
     lt-u
     gt-s
     gt-u
     le-s
     le-u
     ge-s
     ge-u))

(def-type-operators i64
    (eqz
     eq
     ne
     lt-s
     lt-u
     gt-s
     gt-u
     le-s
     le-u
     ge-s
     ge-u))

(def-type-operators f32
    (eq
     ne
     lt
     gt
     le
     ge))

(def-type-operators f64
    (eq
     ne
     lt
     gt
     le
     ge))

;; - type converters - ;;

(def-type-operators i32
    (wrap-i64
     trunc-f32-s
     trunc-f32-u
     trunc-f64-s
     trunc-f64-u
     trunc-sat-f32-s
     trunc-sat-f32-u
     trunc-sat-f64-s
     trunc-sat-f64-u))

(def-type-operators i64
    (extend-i32-s
     extend-i32-u
     trunc-f32-s
     trunc-f32-u
     trunc-f64-s
     trunc-f64-u
     trunc-sat-f32-s
     trunc-sat-f32-u
     trunc-sat-f64-s
     trunc-sat-f64-u))

(def-type-operators f32
    (convert-i32-s
     convert-i32-u
     convert-i64-s
     convert-i64-u
     demote-f64))

(def-type-operators f64
    (convert-i32-s
     convert-i32-u
     convert-i64-s
     convert-i64-u
     promote-f64))

(def-type-operators f32 (reinterpret-f32))
(def-type-operators f64 (reinterpret-f64))
(def-type-operators f32 (reinterpret-i32))
(def-type-operators f64 (reinterpret-i64))

;; - extend operators - ;;

(def-type-operators i32
    (extend8-s
     extend16-s))

(def-type-operators i64
    (extend8-s
     extend16-s
     extend32-s))
