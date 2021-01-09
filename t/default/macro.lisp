(defpackage :watson/t/default/macro
  (:use #:cl
        #:rove
        #:watson/default/macro)
  (:import-from #:watson/env/built-in-func
                #:i32.const
                #:i32.add
                #:i32.sub)
  (:import-from #:watson/env/reserved-word
                #:|if|
                #:|then|
                #:|else|
                #:local
                #:get-local
                #:set-local
                #:block
                #:loop
                #:br
                #:br-if)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|)
  (:import-from #:watson/parser/macro
                #:macroexpand-1.wat))
(in-package :watson/t/default/macro)

(deftest if
  (let ((tests `((:name "then and else case"
                  :test-list (if test a b)
                  :expect (|if| test
                                (|then| a)
                                (|else| b)))
                 (:name "only then case"
                  :test-list (if test a)
                  :expect (|if| test
                                (|then| a)))
                 (:name "only else case"
                  :test-list (if test (progn) a)
                  :expect (|if| test
                                (|then| (progn))
                                (|else| a))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest when
  (let ((tests `((:name "normal case"
                  :test-list (when test a b)
                  :expect (if test
                              (progn a b))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest unless
  (let ((tests `((:name "normal case"
                  :test-list (unless test a b)
                  :expect (if test
                              (progn)
                              (progn a b))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest cond
  (let ((tests `((:name "case without \"t\" condition"
                  :test-list (cond ((a) b c)
                                   ((d) e f))
                  :expect (if (a)
                              (progn b c)
                              (if (d)
                                  (progn e f))))
                 (:name "case with \"t\" condition"
                  :test-list (cond ((a) b c)
                                   ((d) e f)
                                   (t g h))
                  :expect (if (a)
                              (progn b c)
                              (if (d)
                                  (progn e f)
                                  (progn g h))))
                 (:name "case with \"t\" condition in middle"
                  :test-list (cond ((a) b c)
                                   ((d) e f)
                                   (t g h)
                                   ;; The following will be ignored.
                                   ((i) j k))
                  :expect (if (a)
                              (progn b c)
                              (if (d)
                                  (progn e f)
                                  (progn g h)))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest let
  (let ((tests `((:name "normal case"
                  :test-list (let (((a i32) init-a)
                                   (b i32)
                                   ((c i32) init-c))
                               (hoge a b))
                  :expect (progn (local a i32)
                                 (local b i32)
                                 (local c i32)
                                 (set-local a init-a)
                                 (set-local c init-c)
                                 (hoge a b)))
                 (:name "without init-form case"
                  :test-list (let ((a i32)
                                   (b i32))
                               (hoge a b))
                  :expect (progn (local a i32)
                                 (local b i32)
                                 (hoge a b))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest for
  (let ((tests `((:name "normal case"
                  :test-list (for f (:init (init-form)
                                     :break (break-form)
                                     :mod (mod-form))
                                  a b)
                  :expect (progn (init-form)
                                 (block f-block
                                   (loop f-loop
                                         (br-if f-block (break-form))
                                         a
                                         b
                                         (mod-form)
                                         (br f-loop))))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))

(deftest calculation-macro
  (let ((tests `((:name "no argument case"
                  :test-list (i32+)
                  :expect (i32.const 0))
                 (:name "1 argument case"
                  :test-list (i32+ a)
                  :expect (get-local a))
                 (:name "2 more argument case"
                  :test-list (i32+ a b 3)
                  :expect (i32.add a (i32.add b (i32.const 3))))
                 (:name "i32-"
                  :test-list (i32- a b 3)
                  :expect (i32.sub a (i32.sub b (i32.const 3)))))))
    (dolist (tt tests)
      (destructuring-bind (&key name test-list expect) tt
        (testing name
          (ok (equalp (macroexpand-1.wat test-list)
                      expect)))))))
