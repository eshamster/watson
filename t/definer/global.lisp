(defpackage :watson/t/definer/global
  (:use #:cl
        #:rove
        #:watson/definer/global)
  (:import-from #:watson/env/environment
                #:*global-wat-env*
                #:clone-wenvironment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:i32
                #:|i32|
                #:mut
                #:|mut|
                #:|import|
                #:|global|))
(in-package :watson/t/definer/global)

(deftest defglobal.wat
  (let ((tests `((:name "normal case"
                  :init ,(lambda ()
                           (defglobal.wat g js.global (mut i32)))
                  :target-sym g
                  :expect (|global| $g (|import| "js" "global") (|mut| |i32|))))))
    (dolist (tt tests)
      (destructuring-bind (&key name init target-sym expect) tt
        (let ((*global-wat-env* (clone-wenvironment)))
          (testing name
            (funcall init)
            (ok (equalp (funcall (wsymbol-import (intern.wat target-sym)))
                        expect))))))))

