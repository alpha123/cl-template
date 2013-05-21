(defpackage #:cl-template
  (:use #:cl)
  (:nicknames #:clt)
  (:export #:compile-template))

;; Symbols that can be used in templates.
(defpackage #:cl-template-template-symbols
  (:use #:cl))

(defpackage #:cl-template-tests
  (:use #:cl #:cl-template #:fiveam))
