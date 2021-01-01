(defpackage :watson/parser/macro
  (:use #:cl)
  (:import-from #:watson/env/environment
                #:wsymbol-macro-function
                #:intern.wat
                #:*global-wat-env*))
(in-package :watson/parser/macro)

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
