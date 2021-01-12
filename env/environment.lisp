(defpackage :watson/env/environment
  (:use #:cl)
  (:export #:wsymbol-function
           #:wsymbol-macro-function
           #:wsymbol-import
           #:wsymbol-var
           #:*global-wat-env*
           #:intern.wat
           #:clone-wenvironment
           #:with-cloned-wenvironment
           #:wenv-function-symbols
           #:wenv-macro-function-symbols
           #:wenv-import-symbols
           #:wenv-function-body-generators
           #:wenv-import-body-generators
           #:wenv-var-symbols)
  (:import-from #:alexandria
                #:hash-table-values
                #:symbolicate))
(in-package :watson/env/environment)

;; --- wat-symbol --- ;;

(defstruct wat-symbol
  symbol
  import
  function
  macro-function
  var)

(defun clean-wat-symbol-slots (wsymbol without-warn-slot)
  (macrolet ((clean (slot)
               (let ((accessor (symbolicate "WAT-SYMBOL-" slot)))
                 `(progn (when (,accessor wsymbol)
                           (unless (eq ',slot without-warn-slot)
                             (warn "~A has been defined as WAT ~A"
                                   (wat-symbol-symbol wsymbol)
                                   ',slot)))
                         (setf (,accessor wsymbol) nil)))))
    (clean import)
    (clean function)
    (clean macro-function)
    (clean var)))

(defun wsymbol-function (wsymbol)
  (wat-symbol-function wsymbol))

(defsetf wsymbol-function (wsymbol) (func)
  `(progn (clean-wat-symbol-slots ,wsymbol 'function)
          (setf (wat-symbol-function ,wsymbol) ,func)
          ,wsymbol))

(defun wsymbol-macro-function (wsymbol)
  (wat-symbol-macro-function wsymbol))

(defsetf wsymbol-macro-function (wsymbol) (macro-func)
  `(progn (clean-wat-symbol-slots ,wsymbol 'macro-function)
          (setf (wat-symbol-macro-function ,wsymbol) ,macro-func)
          ,wsymbol))

(defun wsymbol-import (wsymbol)
  (wat-symbol-import wsymbol))

(defsetf wsymbol-import (wsymbol) (import)
  `(progn (clean-wat-symbol-slots ,wsymbol 'import)
          (setf (wat-symbol-import ,wsymbol) ,import)
          ,wsymbol))

(defun wsymbol-var (wsymbol)
  (wat-symbol-var wsymbol))

(defsetf wsymbol-var (wsymbol) (var)
  `(progn (clean-wat-symbol-slots ,wsymbol 'var)
          (setf (wat-symbol-var ,wsymbol) ,var)
          ,wsymbol))

;; --- wat-environment --- ;;

(defstruct wat-environment
  (symbol-to-wat-symbols (make-hash-table)))

(defun clone-wenvironment (&optional (wenv *global-wat-env*))
  (let ((result (make-wat-environment)))
    (maphash (lambda (sym wsym)
               (setf (gethash sym (wat-environment-symbol-to-wat-symbols result))
                     (copy-wat-symbol wsym)))
             (wat-environment-symbol-to-wat-symbols wenv))
    result))

(defvar *global-wat-env* (make-wat-environment))

(defmacro with-cloned-wenvironment (&body body)
  `(let ((*global-wat-env* (clone-wenvironment)))
     ,@body))

(defun intern.wat (sym)
  (let ((table (wat-environment-symbol-to-wat-symbols
                *global-wat-env*)))
    (multiple-value-bind (wsym found) (gethash sym table)
      (when found
        (return-from intern.wat wsym))
      (setf (gethash sym table)
            (make-wat-symbol :symbol sym)))))

;; - symbols getter - ;;

(defun extract-wsymbols-by-accessor (wenv wsym-accessor)
  (remove-if (lambda (wsym)
               (not (funcall wsym-accessor wsym)))
             (hash-table-values
              (wat-environment-symbol-to-wat-symbols wenv))))

(defun wenv-specified-symbols (wenv wsym-accessor)
  (mapcar #'wat-symbol-symbol
          (extract-wsymbols-by-accessor wenv wsym-accessor)))

(defun wenv-function-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-function))

(defun wenv-macro-function-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-macro-function))

(defun wenv-import-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-import))

(defun wenv-var-symbols (&optional (wenv *global-wat-env*))
  (wenv-specified-symbols wenv #'wat-symbol-var))

;; - body getter - ;;

(defun wenv-function-body-generators (package &optional (wenv *global-wat-env*))
  (mapcar #'wat-symbol-function
          (remove-if (lambda (wsym)
                       (not (eq package (symbol-package (wat-symbol-symbol wsym)))))
                     (extract-wsymbols-by-accessor wenv #'wat-symbol-function))))

(defun wenv-import-body-generators (package &optional (wenv *global-wat-env*))
  (mapcar #'wat-symbol-import
          (remove-if (lambda (wsym)
                       (not (eq package (symbol-package (wat-symbol-symbol wsym)))))
                     (extract-wsymbols-by-accessor wenv #'wat-symbol-import))))
