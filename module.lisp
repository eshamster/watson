(defpackage :watson/module
  (:use #:cl)
  (:export #:generate-wat-module)
  (:import-from #:watson/env/environment
                #:wenv-import-body-generators
                #:wenv-function-body-generators)
  (:import-from #:watson/definer/export
                #:get-export-body-generators)
  (:import-from #:watson/env/reserved-word
                #:|module|)
  (:import-from #:watson/util/list
                #:clone-list-with-modification)
  (:import-from #:watson/util/package
                #:package-use-list-rec))
(in-package :watson/module)

(defun generate-wat-module% (packages)
  `(|module|
    ,@(mapcar #'funcall (mapcan #'wenv-import-body-generators packages))
    ,@(mapcar #'funcall (mapcan #'wenv-function-body-generators packages))
    ,@(mapcar #'funcall (mapcan #'get-export-body-generators packages))))

(defun generate-wat-module (&rest base-packages)
  "Generate list that can be output by \"princ\" as WAT.
It uses definitions in packages specified by \"base-packages\" and their used pacakges (used packages are recursively searched)."
  (let ((packages (remove-duplicates
                   (append base-packages
                           (mapcan #'package-use-list-rec base-packages)))))
    (clone-list-with-modification
     (generate-wat-module% packages)
     (lambda (elem)
       (typecase elem
         (symbol (symbol-name elem))
         (string (format nil "~S" elem))
         (t elem))))))
