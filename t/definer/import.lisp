(defpackage :watson/t/definer/import
  (:use #:cl
        #:rove
        #:watson/definer/import)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-import
                #:wenv-import-body-generators
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
                  :expect (|import| "fn" "hoge"
                                    (|func| $fuga (|param| |i32|) (|param| |i32|))))
                 (:name "import memory"
                  :init ,(lambda ()
                           (defimport.wat m js.mem (memory 1)))
                  :expect (|import| "js" "mem" (|memory| $m 1))))))
    (dolist (tt tests)
      (destructuring-bind (&key name init expect) tt
        (with-cloned-wenvironment
          (testing name
            (funcall init)
            (let ((imports (wenv-import-body-generators *package*)))
                (ok (= (length imports) 1))
                (ok (equalp (funcall (car imports))
                            expect)))))))))

