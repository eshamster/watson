#|
  This file is a part of watson project.
  Copyright (c) 2020 eshamster (hamgoostar@gmail.com)
|#

#|
  watson: WAT (WebAssembly Text format) Structured ON Lisp

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem watson
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "MIT"
  :depends-on (:alexandria
               :cl-ppcre
               :watson/main)
  :description "watson: WAT (WebAssembly Text format) Structured ON Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op watson/t))))

(defsystem watson/t
  :class :package-inferred-system
  :depends-on (:rove
               "watson/t/default/macro"
               "watson/t/definer/function"
               "watson/t/definer/export"
               "watson/t/definer/global"
               "watson/t/definer/import"
               "watson/t/definer/macro"
               "watson/t/env/built-in-func"
               "watson/t/env/const-func"
               "watson/t/env/environment"
               "watson/t/env/type"
               "watson/t/parser/body-parser"
               "watson/t/parser/macro"
               "watson/t/parser/misc"
               "watson/t/parser/type"
               "watson/t/module"
               "watson/t/util/list"
               "watson/t/util/package"
               "watson/t/util/symbol")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
