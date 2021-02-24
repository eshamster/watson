(defpackage :watson/t/env/environment
  (:use #:cl
        #:rove
        #:watson/env/environment))
(in-package :watson/t/env/environment)

(deftest intern.wat
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge))
          (hoge2 (intern.wat 'hoge))
          (fuga (intern.wat 'fuga)))
      (ok (eq hoge1 hoge2))
      (ok (not (eq hoge1 fuga))))))

(deftest import
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1))
          (imp1 (make-wat-import)))
      (testing "register and check"
        (setf (wsymbol-import hoge1) imp1)
        (ok (eq (wsymbol-import hoge1) imp1)))
      (let ((hoge2 (intern.wat 'hoge2))
            (imp2 (make-wat-import)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-import hoge2) imp2)
        (testing "wenv-import-symbols"
          (let ((syms (wenv-import-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))
        (testing "wenv-import-body-generators"
          (let ((funcs (wenv-import-body-generators *package*)))
            (ok (= (length funcs) 2))))))))

(deftest function
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-function hoge1)
              (make-wat-function :generator (lambda () 1)))
        (ok (wsymbol-function hoge1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-function hoge2)
              (make-wat-function :generator (lambda () 2)))
        (testing "wenv-function-symbols"
          (let ((syms (wenv-function-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))
        (testing "wenv-function-body-generators"
          (let ((funcs (wenv-function-body-generators *package*)))
            (ok (= (length funcs) 2))
            (dolist (func funcs)
              (ok (find (funcall func) '(1 2))))))))))

(deftest macro-function
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1))
          (len-before (length (wenv-macro-function-symbols))))
      (testing "register and check"
        (setf (wsymbol-macro-function hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-macro-function hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-macro-function hoge2)
              (lambda () 2))
        (testing "wenv-macro-function-symbols"
          (let ((syms (wenv-macro-function-symbols)))
            ;; NOTE: There are some default macros.
            ;;       So check difference of length.
            (ok (= (- (length syms) len-before)
                   2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))))))

(deftest var
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1))
          (wvar1 (make-wat-var)))
      (testing "register and check"
        (setf (wsymbol-var hoge1) wvar1)
        (ok (wsymbol-var hoge1) wvar1))
      (let ((hoge2 (intern.wat 'hoge2))
            (wvar2 (make-wat-var)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-var hoge2) wvar2)
        (testing "wenv-var-symbols"
          (let ((syms (wenv-var-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))))))

(deftest global
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-global hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-global hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-global hoge2)
              (lambda () 2))
        (testing "wenv-global-symbols"
          (let ((syms (wenv-global-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))
        (testing "wenv-global-body-generators"
          (let ((funcs (wenv-global-body-generators *package*)))
            (ok (= (length funcs) 2))
            (dolist (func funcs)
              (ok (find (funcall func) '(1 2))))))))))

(deftest export
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-export hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-export hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-export hoge2)
              (lambda () 2))
        (testing "wenv-export-body-generators"
          (let ((funcs (wenv-export-body-generators *package*)))
            (ok (= (length funcs) 2))
            (dolist (func funcs)
              (ok (find (funcall func) '(1 2))))))))))

(deftest exclusive-wat-symbol-slots
  (with-cloned-wenvironment
    (let ((hoge (intern.wat 'hoge)))
      ;; setf to function
      (setf (wsymbol-function hoge)
            (make-wat-function))
      (ok (wsymbol-function hoge))
      (ok (null (wsymbol-import hoge)))
      ;; setf to import
      (setf (wsymbol-import hoge)
            (make-wat-import))
      (ok (null (wsymbol-function hoge)))
      (ok (wsymbol-import hoge))
      ;; setf to export (export is exceptionally not exclusive)
      (setf (wsymbol-export hoge)
            (lambda () 1))
      (ok (wsymbol-import hoge))
      (ok (wsymbol-export hoge)))))

(deftest clone-wenvironment
  (let* ((*global-wat-env* (clone-wenvironment))
         (hoge (intern.wat 'hoge))
         (wfunc1 (make-wat-function)))
    (testing "outer: before"
      (setf (wsymbol-function hoge)
            wfunc1)
      (ok (eq (wsymbol-function hoge) wfunc1)))
    (let* ((*global-wat-env* (clone-wenvironment))
           (hoge (intern.wat 'hoge))
           (wfunc2 (make-wat-function)))
      (testing "inner: before"
        (ok (eq (wsymbol-function hoge) wfunc1)))
      (testing "inner: after"
        (setf (wsymbol-function hoge)
              wfunc2)
        (ok (eq (wsymbol-function hoge) wfunc2))))
    (testing "outer: after"
      (ok (eq (wsymbol-function hoge) wfunc1)))))

(deftest with-cloned-wenvironment
  (with-cloned-wenvironment
    (let ((hoge (intern.wat 'hoge))
          (wfunc1 (make-wat-function)))
      (testing "outer: before"
        (setf (wsymbol-function hoge)
              wfunc1)
        (ok (eq (wsymbol-function hoge) wfunc1)))
      (with-cloned-wenvironment
        (let ((hoge (intern.wat 'hoge))
              (wfunc2 (make-wat-function)))
          (testing "inner: before"
            (ok (eq (wsymbol-function hoge) wfunc1)))
          (testing "inner: after"
            (setf (wsymbol-function hoge)
                  wfunc2)
            (ok (eq (wsymbol-function hoge) wfunc2)))))
      (testing "outer: after"
        (ok (eq (wsymbol-function hoge) wfunc1))))))
