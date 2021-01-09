(defpackage :watson/t/definer/import
  (:use #:cl
        #:rove
        #:watson/definer/import)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-import
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:func
                #:|func|
                #:|import|
                #:memory
                #:|memory|
                #:|param|)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|))
(in-package :watson/t/definer/import)

(deftest defimport.wat
  (let ((tests `((:name "import function"
                  :init ,(lambda ()
                           (defimport.wat fuga fn.hoge (func ((i32) (i32)))))
                  :target-sym fuga
                  :expect (|import| "fn" "hoge"
                                    (|func| $fuga (|param| |i32|) (|param| |i32|))))
                 (:name "import memory"
                  :init ,(lambda ()
                           (defimport.wat m js.mem (memory 1)))
                  :target-sym m
                  :expect (|import| "js" "mem" (|memory| $m 1))))))
    (dolist (tt tests)
      (destructuring-bind (&key name init target-sym expect) tt
        (with-cloned-wenvironment
          (testing name
            (funcall init)
            (ok (equalp (funcall (wsymbol-import (intern.wat target-sym)))
                        expect))))))))

