(defpackage :watson/parser/type
  (:use #:cl)
  (:export #:parse-typeuse)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:watson/parser/misc
                #:parse-var-name)
  (:import-from #:watson/env/reserved-word
                #:|param|
                #:|result|)
  (:import-from #:watson/env/type
                #:convert-type))
(in-package :watson/parser/type)

;; https://webassembly.github.io/spec/core/text/modules.html#text-typeuse
(defun parse-typeuse (typeuse)
  ;; ((param...) (result-type...))
  ;; Ex. (((a i32) (b i32)) (i32))
  ;;  -> return 1. typeuse:   ((|param| a i32) (|param| b i32) (|result| i32))
  ;;            2. variables: (a b)
  ;;            3. types:     (i32 i32)
  (let ((params (car typeuse))
        (results (cadr typeuse)))
    ;; Return parsed list and arg name list
    (values (append (mapcar #'parse-param params)
                    (mapcar #'parse-result-type results))
            (extract-arg-names params)
            (extract-arg-types params))))

(defun parse-param (param)
  (ecase (length param)
    (1 `(|param| ,(convert-type (car param))))
    (2 `(|param| ,(parse-var-name (car param))
                 ,(convert-type (cadr param))))))

(defun extract-arg-names (params)
  (mapcar #'car
          (remove-if (lambda (param)
                       (< (length param) 2))
                     params)))

(defun extract-arg-types (params)
  (mapcar #'car (mapcar #'last params)))

(defun parse-result-type (result-type)
  `(|result| ,(convert-type result-type)))
