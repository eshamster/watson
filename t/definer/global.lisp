(defpackage :watson/t/definer/global
  (:use #:cl
        #:rove
        #:watson/definer/global)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-global
                #:wat-global-type
                #:wenv-global-body-generators
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:mut
                #:|mut|
                #:|import|
                #:|global|)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|))
(in-package :watson/t/definer/global)

(deftest defglobal.wat
  (let ((tests `((:name "normal case"
                  :init ,(lambda ()
                           (defglobal.wat g js.global (mut i32)))
                  :target-sym g
                  :expect (:body (|global| $g (|import| "js" "global") (|mut| |i32|))
                           :type i32)))))
    (dolist (tt tests)
      (destructuring-bind (&key name init target-sym expect) tt
        (with-cloned-wenvironment
          (testing name
            (funcall init)
            (let ((wglobal (wsymbol-global (intern.wat target-sym))))
              (ok (eq (wat-global-type wglobal) (getf expect :type))))
            (let ((funcs (wenv-global-body-generators *package*)))
              (ok (= (length funcs) 1))
              (ok (equalp (funcall (car funcs))
                          (getf expect :body))))))))))

