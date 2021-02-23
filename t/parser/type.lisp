(defpackage :watson/t/parser/type
  (:use #:cl
        #:rove
        #:watson/parser/type)
  (:import-from #:watson/env/reserved-word
                #:|param|
                #:|result|)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|))
(in-package :watson/t/parser/type)

(deftest parse-typeuse
  (let ((tests '((:name "only param case"
                  :input (((a i32) (b i32)) ())
                  :expect1 ((|param| $a |i32|)
                            (|param| $b |i32|))
                  :expect2 (a b)
                  :expect3 (i32 i32))
                 (:name "only result case"
                  :input (() (i32))
                  :expect1 ((|result| |i32|))
                  :expect2 ()
                  :expect3 ())
                 (:name "both case"
                  :input (((a i32) (b i32)) (i32))
                  :expect1 ((|param| $a |i32|)
                            (|param| $b |i32|)
                            (|result| |i32|))
                  :expect2 (a b)
                  :expect3 (i32 i32)))))
    (dolist (tt tests)
      (destructuring-bind (&key name input expect1 expect2 expect3) tt
        (testing name
          (multiple-value-bind (got1 got2 got3)
              (parse-typeuse input)
            (ok (equalp got1 expect1))
            (ok (equalp got2 expect2))
            (ok (equalp got3 expect3))))))))
