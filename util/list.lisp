(defpackage :watson/util/list
  (:use #:cl)
  (:export #:clone-list-with-modification))
(in-package :watson/util/list)

(defun clone-list-with-modification (list fn-each-sym)
  (labels ((rec (rest)
             (if (atom rest)
                 (funcall fn-each-sym rest)
                 (mapcar (lambda (unit)
                           (rec unit))
                         rest))))
    (rec list)))
