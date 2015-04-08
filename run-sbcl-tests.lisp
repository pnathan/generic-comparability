#!/usr/local/bin/sbcl

(require "sb-posix")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defparameter *pwd* (concatenate 'string (sb-posix:getcwd) "/"))

(push *pwd* asdf:*central-registry*)

(ql:quickload :generic-comparability)
(ql:quickload :generic-comparability-test)
(let ((result-status (generic-comparability-test:run-tests)))
  (sb-posix:exit (if result-status 0 1) ))
