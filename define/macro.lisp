(defpackage :watson/define/macro
  (:use #:cl)
  (:export #:defmacro.wat
           #:macroexpand-1.wat
           #:macroexpand.wat)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms)
  (:import-from #:watson/environment
                #:wsymbol-macro-function
                #:wenv-macro-function-symbols
                #:intern.wat
                #:*global-wat-env*))
(in-package :watson/define/macro)

;; TODO: distinguish between default macros and user defined macros

(defmacro defmacro.wat (name lambda-list &body body)
  (with-gensyms (params env)
    `(progn (setf (wsymbol-macro-function (intern.wat ',name))
                  (lambda (,params ,env)
                    (declare (ignorable ,env))
                    (destructuring-bind ,lambda-list (cdr ,params)
                      ,@body)))
            ;; NOTE: Also define with %-suffix as CL macro to mainly try expanding in editor.
            (defmacro ,(symbolicate name "%") ,lambda-list ,@body))))

(defun macro-function-if-expandable (form env)
  (when (atom form)
    (return-from macro-function-if-expandable nil))
  (let ((*global-wat-env* env))
    (let ((wsym (intern.wat (car form))))
      (wsymbol-macro-function wsym))))

(defun macroexpand-1.wat (form &optional (env *global-wat-env*))
  (let ((mf (macro-function-if-expandable form env)))
    (if mf
        (funcall mf form env)
        form)))
 
(defun macroexpand.wat (form &optional (env *global-wat-env*))
  (labels ((rec (form)
             (let ((mf (macro-function-if-expandable form env)))
               (if mf
                   (rec (funcall mf form env))
                   form))))
    (rec form)))
