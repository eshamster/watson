(defpackage :watson/t/parser/type
  (:use #:cl
        #:rove
        #:watson/parser/type)
  (:import-from #:watson/env/reserved-word
                #:|i32|
                #:|param|
                #:|result|))
(in-package :watson/t/parser/type)

(deftest convert-type
  (let ((tests '((:name "type"
                  :input i32
                  :expect |i32|)
                 (:name "ERROR: not type"
                  :input hoge
                  :expect-err t))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect expect-err) tt
        (testing name
          (if expect-err
              (ok (signals (convert-type input)))
              (ok (eq (convert-type input) expect))))))))

(deftest parse-typeuse
  (let ((tests '((:name "only param case"
                  :input (((a i32) (b i32)) ())
                  :expect1 ((|param| $a |i32|)
                            (|param| $b |i32|))
                  :expect2 (a b))
                 (:name "only result case"
                  :input (() (i32))
                  :expect1 ((|result| |i32|))
                  :expect2 ())
                 (:name "both case"
                  :input (((a i32) (b i32)) (i32))
                  :expect1 ((|param| $a |i32|)
                            (|param| $b |i32|)
                            (|result| |i32|))
                  :expect2 (a b)))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect1 expect2) tt
        (testing name
          (multiple-value-bind (got1 got2)
              (parse-typeuse input)
            (ok (equalp got1 expect1))
            (ok (equalp got2 expect2))))))))