(defpackage :watson/parser/type
  (:use #:cl)
  (:export #:convert-type
           #:parse-typeuse)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:watson/parser/misc
                #:parse-arg-name)
  (:import-from #:watson/env/reserved-word
                #:|i32|
                #:|param|
                #:|result|))
(in-package :watson/parser/type)

(defvar *type-table* (make-hash-table))

(defun init-type-table ()
  (setf *type-table* (make-hash-table))
  (dolist (pair '((:i32 |i32|)))
    (destructuring-bind (cl-sym wa-sym) pair
      (setf (gethash cl-sym *type-table*) wa-sym))))

(init-type-table)

(defun convert-type (cl-type-sym)
  (let ((cl-type-keyword (make-keyword cl-type-sym)))
    (multiple-value-bind (got found)
        (gethash cl-type-keyword *type-table*)
      (unless found
        (error "~A is not valid type" cl-type-keyword))
      got)))

;; https://webassembly.github.io/spec/core/text/modules.html#text-typeuse
(defun parse-typeuse (typeuse)
  ;; ((param...) (result-type...))
  ;; Ex. (((a i32) (b i32)) (i32))
  (let ((params (car typeuse))
        (results (cadr typeuse)))
    ;; Return parsed list and arg name list
    (values (append (mapcar #'parse-param params)
                    (mapcar #'parse-result-type results))
            (extract-arg-names params))))

(defun parse-param (param)
  (ecase (length param)
    (1 `(|param| ,(convert-type (car param))))
    (2 `(|param| ,(parse-arg-name (car param))
                 ,(convert-type (cadr param))))))

(defun extract-arg-names (params)
  (mapcar #'car
          (remove-if (lambda (param)
                       (< (length param) 2))
                     params)))

(defun parse-result-type (result-type)
  `(|result| ,(convert-type result-type)))
