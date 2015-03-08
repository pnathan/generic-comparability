;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic-comparability.asd
;;;; paul nathan 2013, 2014
;;;; license: LLGPL
;;;; A mostly conforming implementation of CDR-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:generic-comparability
  :description "CDR-8 implementation"
  :version "1.0"
  :depends-on (#:alexandria)
  :license "LLGPL"
  :author "Paul Nathan"
  :components ((:file "generic-comparability")))

(asdf:defsystem #:generic-comparability-test
  :description "Generic-Comparability test suite"
  :version "1.0"
  :depends-on (#:alexandria
               #:fiveam)
  :license "LLGPL"
  :author "Paul Nathan"
  :components ((:file "generic-comparability-test")))
