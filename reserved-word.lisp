(defpackage :watson/reserved-word
  (:use #:cl)
  (:export #:|module|

           #:|import|
           #:|export|
           #:|func|
           #:func
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
           #:|global.get|
           #:set-global
           #:|global.set|
           #:br
           #:|br|
           #:br-if
           #:|br-if|)
  (:import-from #:watson/util/symbol
                #:sym-to-sym-for-print)
  (:import-from #:alexandria
                #:symbolicate))
(in-package :watson/reserved-word)

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
(defrw get-global |global.get|)
(defrw set-global |global.set|)
(defrw br)
(defrw br-if)
