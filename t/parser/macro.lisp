(defpackage :watson/t/parser/macro
  (:use #:cl
        #:rove
        #:watson/parser/macro)
  (:import-from #:watson/env/environment
                #:*global-wat-env*
                #:clone-wenvironment
                #:wsymbol-macro-function
                #:intern.wat))
(in-package :watson/t/parser/macro)

(defvar *test-wat-env*)

(setup
  (setf *test-wat-env* (clone-wenvironment))
  (let ((*global-wat-env* *test-wat-env*))
    (setf (wsymbol-macro-function (intern.wat 'macro1))
          (lambda (params env)
            (declare (ignore env))
            (destructuring-bind (x y) (cdr params)
              `(hoge ,x ,y))))
    (setf (wsymbol-macro-function (intern.wat 'macro2))
          (lambda (params env)
            (declare (ignore env))
            (destructuring-bind (x y) (cdr params)
              `(macro1 ,x ,y))))))

(deftest macrexpand-1.wat
  (let ((*global-wat-env* *test-wat-env*))
    (let ((tests '((:name "macro case"
                    :input (macro2 1 2)
                    :expect (macro1 1 2))
                   (:name "not macro case"
                    :input (not-macro 1 2)
                    :expect (not-macro 1 2)))))
      (dolist (tt tests)
        (destructuring-bind (&key name input expect) tt
          (testing name
            (ok (equalp (macroexpand-1.wat input)
                        expect))))))))

(deftest macrexpand.wat
  (let ((*global-wat-env* *test-wat-env*))
    (let ((tests '((:name "macro case"
                    :input (macro2 1 2)
                    :expect (hoge 1 2))
                   (:name "not macro case"
                    :input (not-macro 1 2)
                    :expect (not-macro 1 2)))))
      (dolist (tt tests)
        (destructuring-bind (&key name input expect) tt
          (testing name
            (ok (equalp (macroexpand.wat input)
                        expect))))))))
