(defpackage :watson/parser/body-parser
  (:use #:cl)
  (:export #:parse-body)
  (:import-from #:watson/env/built-in-func
                #:built-in-func-p
                #:convert-built-in-func)
  (:import-from #:watson/parser/macro
                #:macroexpand.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-var
                #:*global-wat-env*
                #:intern.wat
                #:clone-wenvironment
                #:wenv-function-symbols
                #:wenv-import-symbols
                #:wenv-var-symbols
                #:wsymbol-macro-function)
  (:import-from #:watson/env/reserved-word
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
                #:|global.get|
                #:set-global
                #:|global.set|
                #:br
                #:|br|
                #:br-if
                #:|br-if|)
  (:import-from #:watson/parser/type
                #:convert-type)
  (:import-from #:watson/parser/misc
                #:parse-var-name))
(in-package :watson/parser/body-parser)

;; --- local environment --- ;;

(defvar *org-global-wat-env* nil)

(defun var-p (sym)
  (some (lambda (syms)
          (find sym syms))
        (list (wenv-var-symbols)
              (wenv-function-symbols)
              (wenv-import-symbols))))

;; --- parser --- ;;

(defun parse-body (body args)
  (let ((*org-global-wat-env* *global-wat-env*)
        (*global-wat-env* (clone-wenvironment)))
    (dolist (arg args)
      (setf (wsymbol-var (intern.wat arg)) t))
    (flatten-progn-all
     (parse-form body))))

(defun flatten-progn-all (body)
  ;; Ex. ((progn 1 2) 3 (progn 4 (progn 5))) -> (1 2 3 4 5)
  (labels ((progn-p (target)
             (and (listp target)
                  (eq (car target) 'progn)))
           (rec (rest)
             (cond ((atom rest)
                    rest)
                   (t (mapcan (lambda (unit)
                                (if (progn-p unit)
                                    (rec (cdr unit))
                                    (list (rec unit))))
                              rest)))))
    (rec body)))

(defun parse-form (form)
  (cond ((atom form)
         (parse-atom form))
        ((special-form-p form)
         (parse-special-form form))
        ((built-in-func-form-p form)
         (parse-built-in-func-form form))
        ((macro-form-p form)
         (parse-macro-form form))
        ((function-call-form-p form)
         (parse-function-call-form form))
        (t (mapcar (lambda (unit)
                     (parse-form unit))
                   form))))

(defun parse-atom (atom)
  (if (var-p atom)
      (parse-var-name atom)
      atom))

;; - special form - ;;

(defun parse-special-form (form)
  (ecase (car form)
    (progn `(progn ,@(mapcar (lambda (unit)
                               (parse-form unit))
                             (cdr form))))
    (local (destructuring-bind (var type) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|local| ,(parse-atom var)
                       ,(convert-type type))))
    (block (destructuring-bind (var &rest rest-form) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|block| ,(parse-atom var)
                       ,@(parse-form rest-form))))
    (loop (destructuring-bind (var &rest rest-form) (cdr form)
             (setf (wsymbol-var (intern.wat var)) t)
             `(|loop| ,(parse-atom var)
                      ,@(parse-form rest-form))))
    (get-local (parse-1-arg-special-form '|get_local| (cdr form)))
    (set-local (parse-1-arg-special-form '|set_local| (cdr form)))
    (get-global (parse-1-arg-special-form '|get_global| (cdr form)))
    (set-global (parse-1-arg-special-form '|set_global| (cdr form)))
    (br (parse-1-arg-special-form '|br| (cdr form)))
    (br-if (parse-1-arg-special-form '|br_if| (cdr form)))))

(defun parse-1-arg-special-form (head args)
  `(,head ,(parse-form (car args))
          ,@(mapcar (lambda (unit)
                      (parse-call-arg unit))
                    (cdr args))))

(defun special-form-p (form)
  (case (car form)
    ((progn local block loop get-local set-local get-global set-global br br-if)
     t)
    (t nil)))

;; - function call arg - ;;

(defun parse-call-arg (arg)
  (if (and (atom arg)
           (var-p arg))
      (parse-form `(get-local ,arg))
      (parse-form arg)))

;; - built-in function form - ;;

(defun parse-built-in-func-form (form)
  `(,(convert-built-in-func (car form))
    ,@(mapcar (lambda (elem)
                (parse-call-arg elem))
              (cdr form))))

(defun built-in-func-form-p (form)
  (built-in-func-p (car form)))

;; - function call form - ;;

(defun parse-function-call-form (form)
  (destructuring-bind (func &rest args) form
    `(|call| ,(parse-atom func)
             ,@(mapcar (lambda (arg)
                         (parse-call-arg arg))
                       args))))

(defun function-call-form-p (form)
  (let ((sym (car form)))
    (some (lambda (syms)
            (find sym syms))
          (list (wenv-function-symbols)
                (wenv-import-symbols)))))

;; - macro - ;;

(defun parse-macro-form (form)
  ;; TODO: consider environment
  (parse-form (macroexpand.wat form *org-global-wat-env*)))

(defun macro-form-p (form)
  (wsymbol-macro-function (intern.wat (car form))))
