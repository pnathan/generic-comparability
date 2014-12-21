;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic-comparability.asd
;;;; paul nathan 2013, 2014
;;;; license: LLGPL
;;;; A mostly conforming implementation of CDR-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:generic-comparability
  :description "CDR-8 implementation"
  :version "0.1"
  :depends-on (#:alexandria)
  :license "LLGPL"
  :author "Paul Nathan"
  :components ((:file "generic-comparability")))
