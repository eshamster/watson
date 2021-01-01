(defpackage :watson/t/util/list
  (:use #:cl
        #:rove
        #:watson/util/list))
(in-package :watson/t/util/list)

(deftest clone-list-with-modification
  (let ((orig   '( 1 ( 2  3)  4))
        (expect '(11 (12 13) 14)))
    (ok (equalp (clone-list-with-modification
                 orig
                 (lambda (x) (+ x 10)))
                expect))
    (ok (= (car orig) 1))))
