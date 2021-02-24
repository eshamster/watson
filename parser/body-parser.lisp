(defpackage :watson/parser/body-parser
  (:use #:cl)
  (:export #:parse-body)
  (:import-from #:watson/env/built-in-func
                #:built-in-func-p
                #:convert-built-in-func
                #:get-arg-types-for-built-in-func)
  (:import-from #:watson/env/const-func
                #:i32.const
                #:i64.const
                #:f32.const
                #:f64.const
                #:const-func-p
                #:convert-const-func
                #:convert-type-to-const-func)
  (:import-from #:watson/parser/macro
                #:macroexpand.wat)
  (:import-from #:watson/env/environment
                #:wsymbol-var
                #:wsymbol-global
                #:wsymbol-function
                #:wsymbol-import
                #:wat-function-arg-types
                #:wat-import-arg-types
                #:make-wat-var
                #:wat-var-type
                #:intern.wat
                #:clone-wenvironment
                #:with-cloned-wenvironment
                #:wenv-function-symbols
                #:wenv-import-symbols
                #:wenv-var-symbols
                #:wenv-global-symbols
                #:wsymbol-macro-function)
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
  (:import-from #:watson/parser/type
                #:convert-type)
  (:import-from #:watson/parser/misc
                #:parse-var-name))
(in-package :watson/parser/body-parser)

;; --- local environment --- ;;

(defvar *org-global-wat-env*)

(defun var-p (sym)
  (some (lambda (syms)
          (find sym syms))
        (list (wenv-var-symbols)
              (wenv-function-symbols)
              (wenv-import-symbols)
              (wenv-global-symbols))))

;; --- parser --- ;;

(defun parse-body (body args arg-types)
  (let ((*org-global-wat-env* (clone-wenvironment)))
    (with-cloned-wenvironment
      (if arg-types
          (progn (assert (= (length args) (length arg-types)) nil
                         "length of args and arg-types are different")
                 (mapcar (lambda (arg type)
                           (setf (wsymbol-var (intern.wat arg))
                                 (make-wat-var :type type)))
                         args arg-types))
          (dolist (arg args)
            (setf (wsymbol-var (intern.wat arg))
                  (make-wat-var :type nil))))
      (flatten-progn-all
       (parse-form body)))))

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
        ((const-func-form-p form)
         (parse-const-func-form form))
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
             (setf (wsymbol-var (intern.wat var))
                   (make-wat-var :type type))
             `(|local| ,(parse-atom var)
                       ,(convert-type type))))
    (block (destructuring-bind (var &rest rest-form) (cdr form)
             (with-cloned-wenvironment
               (setf (wsymbol-var (intern.wat var))
                     (make-wat-var :type nil))
               `(|block| ,(parse-atom var)
                         ,@(parse-form rest-form)))))
    (loop (destructuring-bind (var &rest rest-form) (cdr form)
            (with-cloned-wenvironment
              (setf (wsymbol-var (intern.wat var))
                    (make-wat-var :type nil))
              `(|loop| ,(parse-atom var)
                       ,@(parse-form rest-form)))))
    (get-local (parse-1-arg-special-form '|get_local| (cdr form)))
    (set-local (parse-set-local form))
    (get-global (parse-1-arg-special-form '|get_global| (cdr form)))
    (set-global (parse-1-arg-special-form '|set_global| (cdr form)))
    (br (parse-1-arg-special-form '|br| (cdr form)))
    (br-if (parse-1-arg-special-form '|br_if| (cdr form)))))

(defun parse-set-local (form)
  (destructuring-bind (set-local var value) form
    (assert (eq set-local 'set-local))
    (let ((wvar (wsymbol-var (intern.wat var))))
      (print wvar)
      (assert wvar nil
              "set-local should get var as a first argument")
      `(|set_local| ,(parse-form var)
                    ,(parse-call-arg value (wat-var-type wvar))))))

(defun parse-1-arg-special-form (head args)
  `(,head ,(parse-form (car args))
          ,@(parse-call-args (cdr args) nil)))

(defun special-form-p (form)
  (case (car form)
    ((progn local block loop get-local set-local get-global set-global br br-if)
     t)
    (t nil)))

;; - const functio - ;;

(defun parse-const-func-form (form)
  (assert (= (length form) 2) nil
          "const-func form's length should be 2. form: ~A" form)
  (assert (numberp (cadr form)) nil
          "const-func should get number. form: ~A" form)
  `(,(convert-const-func (car form))
    ,(cadr form)))

(defun const-func-form-p (form)
  (const-func-p (car form)))

;; - function call arg - ;;

(defun parse-call-arg (arg type)
  (if (atom arg)
      (let ((wsymbol (intern.wat arg)))
        (cond ((numberp arg)
               (assert type nil "type should be specified for arg: ~A" arg)
               (parse-form `(,(convert-type-to-const-func type) ,arg)))
              ((wsymbol-var wsymbol)
               (parse-form `(get-local ,arg)))
              ((wsymbol-global wsymbol)
               (parse-form `(get-global ,arg)))
              (t (error "expect local or global variable but ~A is neither" arg))))
      (parse-form arg)))

(defun parse-call-args (args arg-types)
  (when arg-types
    (assert (= (length args) (length arg-types)) nil
            "length of args (~D) and types (~D) should be same"
            (length args) (length arg-types)))
  (mapcar (lambda (arg type)
            (parse-call-arg arg type))
          args
          (if arg-types
              arg-types
              (loop :for x :from 0 :below (length args) :collect nil))))

;; - built-in function form - ;;

(defun parse-built-in-func-form (form)
  (let* ((sym (car form))
         (args (cdr form))
         (arg-types (get-arg-types-for-built-in-func sym args)))
    `(,(convert-built-in-func sym)
      ,@(parse-call-args args arg-types))))

(defun built-in-func-form-p (form)
  (built-in-func-p (car form)))

;; - function call form - ;;

(defun parse-function-call-form (form)
  (destructuring-bind (func &rest args) form
    `(|call| ,(parse-atom func)
             ,@(parse-call-args args (get-function-call-arg-types form)))))

(defun get-function-call-arg-types (form)
  (let* ((wsym (intern.wat (car form)))
         (wfunc (wsymbol-function wsym))
         (wimport (wsymbol-import wsym)))
    (cond (wfunc (wat-function-arg-types wfunc))
          (wimport (wat-import-arg-types wimport)))))

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
