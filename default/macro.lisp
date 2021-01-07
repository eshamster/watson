(defpackage :watson/default/macro
  (:use #:cl)
  (:export #:for
           #:i32+
           #:i32-)
  (:import-from #:watson/env/built-in-func
                #:i32.const
                #:i32.add
                #:i32.sub)
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

(defmacro def-calculation-macro (name const op)
  `(defmacro.wat ,name (&rest args)
     (flet ((parse-arg (arg)
              (cond ((numberp arg)
                     `(,',const ,arg))
                    (t arg))))
       (case (length args)
         (0 `(,',const 0))
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

(def-calculation-macro i32+ i32.const i32.add)
(def-calculation-macro i32- i32.const i32.sub)
