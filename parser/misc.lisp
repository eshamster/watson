(defpackage :watson/parser/misc
  (:use #:cl)
  (:export #:parse-arg-name
           #:parse-mod-nm)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:cl-ppcre
                #:split))
(in-package :watson/parser/misc)

(defun parse-arg-name (arg-name)
  (symbolicate '$ arg-name))

(defun parse-mod-nm (mod-nm)
  "Ex. 'ab.cd -> (\"ab\" \"cd\")"
  (let* ((*print-case* :downcase)
         (mod-nm-str (format nil "~A" mod-nm))
         (splitted (split "\\." mod-nm-str)))
    (unless (= (length splitted) 2)
      (error "mod-nm should be \"xxx.yyy\" but got ~A" mod-nm))
    splitted))
