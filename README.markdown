![.github/workflows/main.yml](https://github.com/eshamster/watson/workflows/.github/workflows/main.yml/badge.svg)

# watson

watson: WAT (WebAssembly Text format) Structured ON Lisp

## Usage

```lisp
(ql:quickload :watson)

(defpackage :sample-package
  (:use :cl
        :watson))
(in-package :sample-package)

(defimport.wat log console.log (func ((i32))))

(defun.wat main () ()
  (let (((x i32) (i32.const 5)))
    (log (factorial x))))

(defun.wat factorial ((x i32)) (i32)
  (let ((result i32))
    (if (i32.ge-u (i32.const 1) x)
        (set-local result (i32.const 1))
        (progn (i32.mul x
                        (factorial (i32.sub x (i32.const 1))))
               (set-local result)))
    (get-local result)))

(defexport.wat exported-func (func main))

(let ((*print-pretty* t))
  (princ (generate-wat-module *package*)))
```

The result is the following.

```wat
(module (import "console" "log" (func $LOG (param i32)))
 (func $MAIN (local $X i32) (set_local $X (i32.const 5))
  (call $LOG (call $FACTORIAL (get_local $X))))
 (func $FACTORIAL (param $X i32) (result i32) (local $RESULT i32)
  (if (i32.ge_u (i32.const 1) (get_local $X))
   (then (set_local $RESULT (i32.const 1)))
   (else
    (i32.mul (get_local $X)
     (call $FACTORIAL (i32.sub (get_local $X) (i32.const 1))))
    (set_local $RESULT)))
  (get_local $RESULT))
 (export "exported_func" (func $MAIN)))
```

Cf. Format above by hand

```wat
(module
  (import "console" "log" (func $LOG (param i32)))
  (func $MAIN
    (local $X i32)
    (set_local $X (i32.const 5))
    (call $LOG (call $FACTORIAL (get_local $X))))
  (func $FACTORIAL (param $X i32) (result i32)
    (local $RESULT i32)
    (if (i32.ge_u (i32.const 1) (get_local $X))
      (then (set_local $RESULT (i32.const 1)))
      (else
        (i32.mul (get_local $X)
        (call $FACTORIAL (i32.sub (get_local $X) (i32.const 1))))
        (set_local $RESULT)))
    (get_local $RESULT))
  (export "exported_func" (func $MAIN)))
```

## Installation

## Author

- eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2020 eshamster (hamgoostar@gmail.com)

## License

Licensed under the MIT License.
