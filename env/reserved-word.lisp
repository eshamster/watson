(defpackage :watson/env/reserved-word
  (:use #:cl)
  (:export #:|module|

           #:|import|
           #:|export|
           #:|func|
           #:func
           #:|call|
           #:|memory|
           #:memory
           #:|global|
           #:global
           #:|mut|
           #:mut
           #:|param|
           #:|result|

           #:|if|
           #:|then|
           #:then
           #:|else|
           #:else

           #:|block|
           #:block
           #:|loop|
           #:loop

           #:|i32|

           #:local
           #:|local|
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
           #:|br-if|)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/env/reserved-word)

(defmacro defrw (sym &optional sym-for-print)
  `(progn ,(when (eq (symbol-package sym)
                     *package*)
             `(defvar ,sym nil))
          (defvar ,(if sym-for-print
                       sym-for-print
                       (sym-to-sym-for-print sym))
            nil)))

(defrw module)

(defrw import)
(defrw export)
(defrw func)
(defrw call)
(defrw memory)
(defrw global)
(defrw mut)
(defrw param)
(defrw result)

(defrw if)
(defrw then)
(defrw else)

(defrw block)
(defrw loop)

;; - type - ;;

(defrw i32)

;; - special form - ;;

(defrw local)
(defrw get-local)
(defrw set-local)
(defrw get-global)
(defrw set-global)
(defrw br)
(defrw br-if)
