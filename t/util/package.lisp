(defpackage :watson/t/util/package
  (:use #:cl
        #:rove
        #:watson/util/package))
(in-package :watson/t/util/package)

;; e -> d -> b -> a
;;        -> c

(defpackage :watson/t/util/package/test-a
  (:use))
(defpackage :watson/t/util/package/test-b
  (:use #:watson/t/util/package/test-a))
(defpackage :watson/t/util/package/test-c
  (:use))
(defpackage :watson/t/util/package/test-d
  (:use #:watson/t/util/package/test-b
        #:watson/t/util/package/test-c))
(defpackage :watson/t/util/package/test-e
  (:use #:watson/t/util/package/test-d))

(deftest package-use-list-rec
  (let ((result (package-use-list-rec
                 (find-package "WATSON/T/UTIL/PACKAGE/TEST-E"))))
    (ok (= (length result) 4))
    (dolist (package (list (find-package "WATSON/T/UTIL/PACKAGE/TEST-A")
                           (find-package "WATSON/T/UTIL/PACKAGE/TEST-B")
                           (find-package "WATSON/T/UTIL/PACKAGE/TEST-C")
                           (find-package "WATSON/T/UTIL/PACKAGE/TEST-D")))
      (ok (find package result)))))
