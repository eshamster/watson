(defpackage :watson/module
  (:use #:cl)
  (:export #:generate-wat-module)
  (:import-from #:watson/environment
                #:wenv-import-body-generators
                #:wenv-function-body-generators)
  (:import-from #:watson/define/export
                #:get-export-body-generators)
  (:import-from #:watson/reserved-word
                #:|module|)
  (:import-from #:watson/utils
                #:clone-list-with-modification))
(in-package :watson/module)

(defun generate-wat-module% ()
  `(|module|
    ,@(mapcar #'funcall (wenv-import-body-generators))
    ,@(mapcar #'funcall (wenv-function-body-generators))
    ,@(mapcar #'funcall (get-export-body-generators))))

(defun generate-wat-module ()
  (let ((str-list (clone-list-with-modification
                   (generate-wat-module%)
                   (lambda (elem)
                     (typecase elem
                       (symbol (symbol-name elem))
                       (string (format nil "~S" elem))
                       (t elem))))))
    str-list))

