(defpackage :watson/util/symbol
  (:use #:cl)
  (:export #:symbol-to-string
           #:sym-to-sym-for-print)
  (:import-from #:cl-ppcre
                #:regex-replace-all))
(in-package :watson/util/symbol)

(defun symbol-to-string (sym)
  (string-downcase
   (regex-replace-all "-" (symbol-name sym) "_")))

(defun sym-to-sym-for-print (sym)
    (intern (string-downcase
             (regex-replace-all "-" (symbol-name sym) "_"))))
