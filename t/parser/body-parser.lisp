(defpackage :watson/t/parser/body-parser
  (:use #:cl
        #:rove
        #:watson/parser/body-parser
        #:watson/default/macro)
  (:import-from #:watson/env/built-in-func
                #:i32.add
                #:convert-built-in-func)
  (:import-from #:watson/env/const-func
                #:i32.const
                #:i64.const
                #:convert-const-func)
  (:import-from #:watson/env/environment
                #:with-cloned-wenvironment
                #:wsymbol-function
                #:wsymbol-global
                #:wsymbol-macro-function
                #:make-wat-function
                #:intern.wat)
  (:import-from #:watson/env/reserved-word
                #:|call|
                #:|local|
                #:local
                #:|block|
                #:block
                #:|loop|
                #:loop
                #:get-local
                #:|get_local|
                #:set-local
                #:|set_local|
                #:get-global
                #:|get_global|
                #:set-global
                #:|set_global|
                #:br
                #:|br|
                #:br-if
                #:|br_if|)
  (:import-from #:watson/env/type
                #:i32
                #:|i32|
                #:i64))
(in-package :watson/t/parser/body-parser)

(deftest parse-atom
  (let ((tests `((:name "parse arg"
                  :input (:body a
                          :args (a b))
                  :expect $a)
                 (:name "parse func symbol"
                  :init ,(lambda ()
                           (setf (wsymbol-function (intern.wat 'hoge))
                                 (make-wat-function)))
                  :input (:body hoge
                          :args (a b))
                  :expect $hoge)
                 (:name "parse not variable"
                  :input (:body c
                          :args (a b))
                  :expect c))))
    (dolist (tt tests)
      (with-cloned-wenvironment
        (destructuring-bind (&key name init input expect) tt
          (destructuring-bind (&key body args) input
            (when init
              (funcall init))
            (testing name
              (ok (equalp (parse-body body args)
                          expect)))))))))

(deftest parse-special-form
  (let ((tests `((:name "progn"
                  :input (:body ((progn 1 (progn 2 3) 4))
                          :args ())
                  :expect (1 2 3 4))
                 (:name "local"
                  :input (:body ((local a i32)
                                 a
                                 b)
                          :args ())
                  :expect ((|local| $a |i32|)
                           $a
                           b))
                 (:name "block"
                  :input (:body ((block blk
                                   blk)
                                 blk)
                          :args ())
                  :expect ((|block| $blk
                                    $blk)
                           blk))
                 (:name "loop"
                  :input (:body ((loop lp
                                       lp)
                                 lp)
                          :args ())
                  :expect ((|loop| $lp
                                    $lp)
                           lp))
                 (:name "get-local"
                  :input (:body ((local a i32)
                                 (get-local a)
                                 (get-local b))
                          :args (b))
                  :expect ((|local| $a |i32|)
                           (|get_local| $a)
                           (|get_local| $b)))
                 (:name "set-local"
                  :input (:body ((local a i32)
                                 (local b i32)
                                 (set-local a b))
                          :args ())
                  :expect ((|local| $a |i32|)
                           (|local| $b |i32|)
                           (|set_local| $a (|get_local| $b))))
                 ;; NOTE: The parser doesn't distinguish local and global variable.
                 (:name "get-global"
                  :input (:body (get-global a)
                          :args (a))
                  :expect (|get_global| $a))
                 (:name "set-global"
                  :input (:body (set-global a b)
                          :args (a b))
                  :expect (|set_global| $a (|get_local| $b)))
                 (:name "br"
                  :input (:body (block blk
                                  (br blk))
                          :args ())
                  :expect (|block| $blk
                                   (|br| $blk)))
                 (:name "br-if"
                  :input (:body (block blk
                                  (br-if blk))
                          :args ())
                  :expect (|block| $blk
                                   (|br_if| $blk))))))
    (dolist (tt tests)
      (with-cloned-wenvironment
        (destructuring-bind (&key name input expect) tt
          (destructuring-bind (&key body args) input
            (testing name
              (ok (equalp (parse-body body args)
                          expect)))))))))

(deftest parse-built-in-func-form
  (let ((|i32.add| 'watson/env/built-in-func::|i32.add|))
    (let ((tests `((:name "parse normal case"
                    :input (:body (i32.add (get-local a) (get-local b))
                            :args (a b))
                    :expect (,|i32.add| (|get_local| $a) (|get_local| $b)))
                   (:name "get-local is inserted"
                    :input (:body (i32.add a b)
                            :args (a b))
                    :expect (,|i32.add| (|get_local| $a) (|get_local| $b)))
                   (:name "<type>.const is inserted"
                    :input (:body (i32.add 1 (i32.const 2))
                            :args ())
                    :expect (,|i32.add| (,(convert-const-func 'i32.const) 1)
                                        (,(convert-const-func 'i32.const) 2))))))
      (dolist (tt tests)
        (with-cloned-wenvironment
          (destructuring-bind (&key name input expect) tt
            (destructuring-bind (&key body args) input
              (testing name
                (ok (equalp (parse-body body args)
                            expect))))))))))

(deftest parse-macro-form
  (let ((|i32.add| 'watson/env/built-in-func::|i32.add|))
    (let ((tests `((:name "parse normal case"
                    :init ,(lambda ()
                             (setf (wsymbol-macro-function (intern.wat 'hoge))
                                   (lambda (params env)
                                     (declare (ignore env))
                                     (destructuring-bind (a b)
                                         (cdr params)
                                       `(i32.add ,a ,b)))))
                    :input (:body (hoge a b)
                            :args (a b))
                    :expect (,|i32.add| (|get_local| $a) (|get_local| $b))))))
      (dolist (tt tests)
        (with-cloned-wenvironment
          (destructuring-bind (&key name init input expect) tt
            (destructuring-bind (&key body args) input
              (when init
                (funcall init))
              (testing name
                (ok (equalp (parse-body body args)
                            expect))))))))))

(deftest parse-function-call-form
  (let ((tests `((:name "parse normal case"
                  :init ,(lambda ()
                           (setf (wsymbol-function (intern.wat 'hoge))
                                 (make-wat-function)))
                  :input (:body (hoge (get-local a) (get-local b))
                          :args (a b))
                  :expect (|call| $hoge (|get_local| $a) (|get_local| $b)))
                 (:name "get-local is inserted"
                  :init ,(lambda ()
                           (setf (wsymbol-function (intern.wat 'hoge))
                                 (make-wat-function)))
                  :input (:body (hoge a b)
                          :args (a b))
                  :expect (|call| $hoge (|get_local| $a) (|get_local| $b)))
                 (:name "get-global is inserted"
                  :init ,(lambda ()
                           (setf (wsymbol-function (intern.wat 'hoge))
                                 (make-wat-function))
                           (setf (wsymbol-global (intern.wat 'g))
                                 (lambda () 2)))
                  :input (:body (hoge a g)
                          :args (a))
                  :expect (|call| $hoge (|get_local| $a) (|get_global| $g)))
                 (:name "<type>.const is inserted"
                  :init ,(lambda ()
                           (setf (wsymbol-function (intern.wat 'hoge))
                                 (make-wat-function :arg-types '(i32 i64 i32))))
                  :input (:body (hoge 1 2 (i32.const 3))
                          :args ())
                  :expect (|call| $hoge
                                  (,(convert-const-func 'i32.const) 1)
                                  (,(convert-const-func 'i64.const) 2)
                                  (,(convert-const-func 'i32.const) 3))))))
    (dolist (tt tests)
      (with-cloned-wenvironment
        (destructuring-bind (&key name init input expect) tt
          (destructuring-bind (&key body args) input
            (when init
              (funcall init))
            (testing name
              (ok (equalp (parse-body body args)
                          expect)))))))))
