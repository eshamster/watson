(defpackage :watson/t/definer/macro
  (:use #:cl
        #:rove
        #:watson/definer/macro)
  (:import-from #:watson/env/environment
                #:*global-wat-env*
                #:clone-wenvironment)
  (:import-from #:watson/parser/macro
                #:macroexpand.wat))
(in-package :watson/t/definer/macro)

(deftest defmacro.wat
  (let ((tests `((:name "normal case"
                  :init ,(lambda ()
                           (watson/definer/macro::defmacro.wat% hoge (x y)
                             `(fuga ,x ,y)))
                  :test-list (hoge a b)
                  :expect (fuga a b)))))
    (dolist (tt tests)
      (destructuring-bind (&key name init test-list expect) tt
        (let ((*global-wat-env* (clone-wenvironment)))
          (testing name
            (funcall init)
            (ok (equalp (macroexpand.wat test-list)
                        expect))))))))

