(defpackage :watson/utils
  (:use #:cl)
  (:export #:clone-list-with-modification
           #:symbol-to-string
           #:sym-to-sym-for-print)
  (:import-from #:cl-ppcre
                #:regex-replace-all))
(in-package :watson/utils)

(defun clone-list-with-modification (list fn-each-sym)
  (labels ((rec (rest)
             (if (atom rest)
                 (funcall fn-each-sym rest)
                 (mapcar (lambda (unit)
                           (rec unit))
                         rest))))
    (rec list)))

(defun symbol-to-string (sym)
  (string-downcase
   (regex-replace-all "-" (symbol-name sym) "_")))

(defun sym-to-sym-for-print (sym)
    (intern (string-downcase
             (regex-replace-all "-" (symbol-name sym) "_"))))
