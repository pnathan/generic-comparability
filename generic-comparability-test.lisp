;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic-comparability-test.lisp test set for
;;;; generic-comparability, an implementation of CDR-8
;;;; (C) 2015 Paul Nathan, License - LLGPL.o
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:generic-comparability-test
  (:use #:cl #:generic-comparability #:fiveam)
  (:export #:run-tests))
(in-package #:generic-comparability-test)

(def-suite test-set :description "General test set")
(in-suite test-set)

(test issue-1)
;; Addresses https://github.com/pnathan/generic-comparability/issues/1
(test issue-1-numbers

  ;; numerics
  (is
   (eq (case (compare 4 5)
         (< :less-than)
         (> :greater-than)
         (/= :different))
       :less-than))
  (is
   (eq (case (compare 5 4)
         (< :less-than)
         (> :greater-than)
         (/= :different))
       :greater-than))
  (is
   (eq (case (compare 4 4)
         (< :less-than)
         (> :greater-than)
         (/= :different)
         (= :same))
       :same))

  (is
   (eq (case (compare pi pi)
         (< :less-than)
         (> :greater-than)
         (= :equal)
         (/= :different))
       :equal))

  (is (eq (compare 3.0s10 3.000001s10 :floating-compare t )
          '<))

  (is (eq (compare 3.0s10 3.00001s10 :floating-compare nil )
          '<)))

(test issue-1-sequences
  ;; sequences
  ;; these have no ordering relationship.
  (is (eq (compare '(j a z z) '(j a z z))
          '/=))
  (is (eq (compare '(j a z z) '(r o c k))
          '/=))
  (is (eq (compare #(b a s s) #(g u i t a r))
          '/=))
  (is (eq (compare #(b a s s) #(b a s s))
          '/=)))

(test issue-1-symbols
  ;; symbols
  (is (eq (case (compare 'monkey 'bear)
            (< :less-than)
            (> :greater-than)
            (/= :different)
            (= :same))
          :different))

  (is (eq (case (compare 'snake 'snake)
            (< :less-than)
            (> :greater-than)
            (/= :different)
            (= :same))
          :same)))

(test issue-1-characters
  ;; characters
  (is (eq (compare #\C #\C) '=)
      "C C")
  (is (eq (compare #\C #\c) '<)
      "C c")

  (is (eq (compare #\C #\c :case-sensitive nil) '=)
      "C c case-sensitive nil")

  (is (eq (compare #\C #\D :case-sensitive nil) '<)
      "C D case-sensitive nil"))

(test issue-1-strings
  ;; strings
  (is (eq (compare "asd" "asd")
          '=)
      "asd asd")
  (is (eq (compare "asd" "ASD")
          '>)
      "asd ASD")
  (is (eq (compare "ASD" "asd")
          '<)
      "ASD asd")
  (is (eq (compare "bbb" "aaa")
          '>)
      "bbb aaa")
  (is (eq (compare "bbb" "ccc")
          '<)
      "bbb ccc")

  (is (eq (compare "asd" "ASD" :case-sensitive nil)
          '=)
      "asd ASD case-sensitive nil")
  (is (eq (compare "asd" "ASDf" :case-sensitive nil)
          '<)
      "asd ASDf case-sensitive nil"))

(test comparability
  (test lt-compare
    (is (lt 0 100))
    (is (not (lt 20 20)))
    (is (not (lt 40 0))))

  (test lte-compare
    (is (lte 0 200))
    (is (lte 200 200))
    (is (not (lte 50 -3))))

  (test gt-compare
    (is (gt 200 0))
    (is (not (gt -800 -800)))
    (is (not (gt -800 0))))

  (test gte-compare
    (is (gte 100 0))
    (is (gte 200 200))
    (is (not (gte 0 300)))))

;; example from the spec.
(defstruct foo a s d)

(test incomparable
  (signals incomparable-object
     (lte (make-array 3 :initial-element 0)
          (vector 1 2 42)))
  (signals incomparable-object
     (lte (make-foo :a 0 :d "I am a FOO")
          (make-foo :a 42 :d "I am a foo"))))

(test verify-generic-hash-code
  (is (eql (hash-code 10)
           (hash-code 10)))

  (is (not (eql (hash-code "One Thing")
                (hash-code "1d10t"))))

  (is (not (eql (hash-code 0)
            (hash-code "String"))))

  (is (eql (hash-code "A string")
           (hash-code "A string"))))

(defun run-tests ()
  (let ((results (run 'test-set)))
    (explain! results)
    (results-status results)))
