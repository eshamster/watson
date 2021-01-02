(defpackage :watson/util/package
  (:use #:cl)
  (:export #:package-use-list-rec)
  (:import-from #:alexandria
                #:hash-table-keys))
(in-package :watson/util/package)

(defun package-use-list-rec (base-package)
  (labels ((rec (package found)
             (unless (gethash package found)
               (setf (gethash package found) t)
               (dolist (used-package (package-use-list package))
                 (rec used-package found)))))
    (let ((found (make-hash-table)))
      (rec base-package found)
      (remove base-package (hash-table-keys found)))))
