(defpackage :watson/default/macro
  (:use #:cl)
  (:export #:for

           #:i32+
           #:i32-
           #:i32*
           #:i32s/
           #:i32u/

           #:i64+
           #:i64-
           #:i64*
           #:i64s/
           #:i64u/

           #:f32+
           #:f32-
           #:f32*
           #:f32/

           #:f64+
           #:f64-
           #:f64*
           #:f64/)
  (:import-from #:watson/env/built-in-func
                #:i32.const
                #:i32.add
                #:i32.sub
                #:i32.mul
                #:i32.div-s
                #:i32.div-u
                #:i64.add
                #:i64.sub
                #:i64.mul
                #:i64.div-s
                #:i64.div-u
                #:f32.add
                #:f32.sub
                #:f32.mul
                #:f32.div
                #:f64.add
                #:f64.sub
                #:f64.mul
                #:f64.div)
  (:import-from #:watson/definer/macro
                #:defmacro.wat)
  (:import-from #:watson/env/reserved-word
                #:|if|
                #:|then|
                #:then
                #:|else|
                #:else
                #:local
                #:block
                #:loop
                #:br
                #:br-if
                #:get-local
                #:set-local)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/default/macro)

;; --- control macros --- ;;

(defmacro.wat if (test-form then-form &optional else-form)
  `(|if| ,test-form
         ,@(if else-form
               `((|then| ,then-form)
                 (|else| ,else-form))
               `((|then| ,then-form)))))

(defmacro.wat when (test-form &body form)
  `(if ,test-form
       (progn ,@form)))

(defmacro.wat unless (test-form &body form)
  `(if ,test-form
       (progn)
       (progn ,@form)))

(defmacro.wat cond (&rest clauses)
  (labels ((rec (rest-clauses)
             (unless rest-clauses
               (return-from rec))
             (let* ((clause (car rest-clauses))
                    (test-form (car clause))
                    (form (cdr clause)))
               (cond ((eq test-form 't)
                      `(progn ,@form))
                     ((cdr rest-clauses)
                      `(if ,test-form
                           (progn ,@form)
                           ,(rec (cdr rest-clauses))))
                     (t
                      `(if ,test-form
                           (progn ,@form)))))))
    (rec clauses)))

(defmacro.wat let (var-forms &body body)
  ;; Ex. (let (((i i32) (i32.const 0))
  ;;           (j i32))
  ;;       ...)
  ;; Limitations:
  ;; - Can use this only at head of function.
  ;; - A variable is not hidden to others.
  ;; - The scope is same to "local" operator.
  ;;
  ;; Probably, this should be defined as a special form.
  `(progn ,@(mapcar (lambda (var-form)
                      (let ((var-type (if (listp (car var-form))
                                          (car var-form)
                                          var-form)))
                        (destructuring-bind (var type) var-type
                          `(local ,var ,type))))
                    var-forms)
          ,@(mapcan (lambda (var-form)
                      (when (listp (car var-form))
                        (let ((var-type (car var-form))
                              (init (cadr var-form)))
                         (destructuring-bind (var type) var-type
                           (declare (ignore type))
                           `((set-local ,var ,init))))))
                   var-forms)
          ,@body))

(defmacro.wat let* (var-form &body body)
  `(let ,var-form ,@body))

;; experimental
(defmacro.wat for (for-name (&key init break mod) &body body)
  (let ((block-name (symbolicate for-name "-BLOCK"))
        (loop-name  (symbolicate for-name "-LOOP")))
    `(progn ,init
            (block ,block-name
              (loop ,loop-name
                     (br-if ,block-name ,break)
                    ,@body
                    ,mod
                     (br ,loop-name))))))

;; --- calculation macros --- ;;

(defmacro def-calculation-macro (name const initial-value op)
  `(defmacro.wat ,name (&rest args)
     (flet ((parse-arg (arg)
              (cond ((numberp arg)
                     `(,',const ,arg))
                    (t arg))))
       (case (length args)
         (0 `(,',const ,',initial-value))
         ;; XXX: Strictry speaking, require to judge
         ;; if the "arg" is a local variable,
         ;; before adding "get-local"
         (1 `(get-local ,(car args)))
         (t (labels ((rec (rest-args)
                       (let ((head (car rest-args))
                             (rest (cdr rest-args)))
                         (if rest
                             `(,',op ,(parse-arg head)
                                     ,(rec rest))
                             (parse-arg head)))))
              (rec args)))))))

(def-calculation-macro i32+ i32.const 0 i32.add)
(def-calculation-macro i32- i32.const 0 i32.sub)
(def-calculation-macro i32* i32.const 1 i32.mul)
(def-calculation-macro i32s/ i32.const 1 i32.div-s)
(def-calculation-macro i32u/ i32.const 1 i32.div-u)

(def-calculation-macro i64+ i64.const 0 i64.add)
(def-calculation-macro i64- i64.const 0 i64.sub)
(def-calculation-macro i64* i64.const 1 i64.mul)
(def-calculation-macro i64s/ i64.const 1 i64.div-s)
(def-calculation-macro i64u/ i64.const 1 i64.div-u)

(def-calculation-macro f32+ f32.const 0 f32.add)
(def-calculation-macro f32- f32.const 0 f32.sub)
(def-calculation-macro f32* f32.const 1 f32.mul)
(def-calculation-macro f32/ f32.const 1 f32.div)

(def-calculation-macro f64+ f64.const 0 f64.add)
(def-calculation-macro f64- f64.const 0 f64.sub)
(def-calculation-macro f64* f64.const 1 f64.mul)
(def-calculation-macro f64/ f64.const 1 f64.div)
