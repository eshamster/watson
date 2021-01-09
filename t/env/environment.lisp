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
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-import hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-import hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-import hoge2)
              (lambda () 2))
        (testing "wenv-import-symbols"
          (let ((syms (wenv-import-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))
        (testing "wenv-import-body-generators"
          (let ((funcs (wenv-import-body-generators *package*)))
            (ok (= (length funcs) 2))
            (dolist (func funcs)
              (ok (find (funcall func) '(1 2))))))))))

(deftest function
  (with-cloned-wenvironment
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-function hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-function hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-function hoge2)
              (lambda () 2))
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
    (let ((hoge1 (intern.wat 'hoge1)))
      (testing "register and check"
        (setf (wsymbol-var hoge1)
              (lambda () 1))
        (ok (eq (funcall (wsymbol-var hoge1))
                1)))
      (let ((hoge2 (intern.wat 'hoge2)))
        (intern.wat 'hoge3) ; this won't be got
        (setf (wsymbol-var hoge2)
              (lambda () 2))
        (testing "wenv-var-symbols"
          (let ((syms (wenv-var-symbols)))
            (ok (= (length syms) 2))
            (dolist (sym '(hoge1 hoge2))
              (ok (find sym syms)))))))))

(deftest clone-wenvironment
  (let* ((*global-wat-env* (clone-wenvironment))
         (hoge (intern.wat 'hoge)))
    (testing "outer: before"
      (setf (wsymbol-function hoge)
            (lambda () 100))
      (ok (eq (funcall (wsymbol-function hoge))
              100)))
    (let* ((*global-wat-env* (clone-wenvironment))
           (hoge (intern.wat 'hoge)))
      (testing "inner: before"
        (ok (eq (funcall (wsymbol-function hoge))
                100)))
      (testing "inner: after"
        (setf (wsymbol-function hoge)
              (lambda () 200))
        (ok (eq (funcall (wsymbol-function hoge))
                200))))
    (testing "outer: after"
      (ok (eq (funcall (wsymbol-function hoge))
              100)))))

(deftest with-cloned-wenvironment
  (with-cloned-wenvironment
    (let ((hoge (intern.wat 'hoge)))
      (testing "outer: before"
        (setf (wsymbol-function hoge)
              (lambda () 100))
        (ok (eq (funcall (wsymbol-function hoge))
                100)))
      (with-cloned-wenvironment
        (let ((hoge (intern.wat 'hoge)))
          (testing "inner: before"
            (ok (eq (funcall (wsymbol-function hoge))
                    100)))
          (testing "inner: after"
            (setf (wsymbol-function hoge)
                  (lambda () 200))
            (ok (eq (funcall (wsymbol-function hoge))
                    200)))))
      (testing "outer: after"
        (ok (eq (funcall (wsymbol-function hoge))
                100))))))
