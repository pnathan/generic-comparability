;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic-comparability.lisp
;;;; paul nathan 2013, 2014
;;;; license: LLGPL
;;;; A mostly conforming implementation of CDR-8
;;;; http://cdr.eurolisp.org/document/8/cleqcmp.html
;;;;
;;;; The documentation is largely cribbed from CDR-8 itself, in order
;;;; to provide corresponding understanding of the intent.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:generic-comparability
  (:use #:cl #:alexandria)
  (:export
   #:equals

   #:compare

   #:incomparable-object

   #:lt
   #:lte
   #:gt
   #:gte
   #:lessp
   #:not-lessp
   #:greaterp
   #:not-greaterp

   #:hash-code))

(in-package #:generic-comparability)


(defgeneric equals (a b &rest keys &key recursive key &allow-other-keys)
  (:documentation
   "a b -- Common Lisp objects.
recursive -- a generalized boolean; default is NIL.
result -- a boolean.
keys -- a list (as per the usual behavior).
by-key -- a generalized boolean; default is T.
by-values -- a generalized boolean; default is T.
check-properties -- a generalized boolean; default is NIL.
case-sensitive -- a generalized boolean; default is T.

Description:

The EQUALS generic functions defines methods to test for `equality` of
two objects a and b. When two objects a and b are EQUALS under an
appropriate and context-dependent notion of `equality`, then the
function returns T as result; otherwise EQUALS returns NIL as result.

If the argument recursive is T, then EQUALS may recurse down the
`structure` of a and b. The description of each known method contains
the relevant information about its recursive dependent behavior.

EQUALS provides some default behavior, but it is intended mostly as a
hook for users. As such, it is allowed to add keyword arguments to
user-defined EQUALS methods, as the &key and &allow-other-keys
lambda-list markers imply."))


(defmethod equals ((a t) (b t) &rest keys)
  (declare (ignore keys))
  (equalp a b))

(defmethod equals ((a float) (b float)
                   &rest keys
                   &key
                     floating-compare
                     (max-relative-diff 1.19e-7))
  "EQUALS float float &key (floating-compare nil) (max-relative-diff 1.19e7)

If floating-compare is true, then the floating comparison algorithm is used, as opposed to `=`.

The floating comparison is derived from here:
http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/

The Internet Archive has a copy of this URL.

The implemented algorithm is AlmostEqualRelative. It is not the best
possible float equality algorithm, but is simple and
understandable (and easy to code). Improvements with citations from
the literature are welcome.

Observe the following comparison and beware:
CL-USER> (equals 1.0 1.0000001 :floating-compare t)
NIL
CL-USER> (equals 1.0 1.00000001 :floating-compare t)
T
"
  (declare (ignore keys))

  (if floating-compare
      (let ((diff (abs (- a b)))
            (a (abs a))
            (b (abs b)))
        (<= diff (* (max a b) max-relative-diff)))
      ;; If we decided to go with error-prone =...
      (= a b)))

(defmethod equals ((a number) (b number) &rest keys)
  (declare (ignore keys))
  (= a b))

(defmethod equals ((a cons) (b cons) &rest keys)
  (declare (ignore keys))
  (tree-equal a b :test #'equals))


(defmethod  equals ((a character) (b character)
                    &rest keys
                    &key (case-sensitive t))
  (declare (ignore keys))
  (if case-sensitive
      (char= a b)
      (char-equal a b)))


(defmethod equals ((a string) (b string)
                   &rest keys
                   &key (case-sensitive t))
  (declare (ignore keys))
  (if case-sensitive
      (string= a b)
      (string-equal a b)))


(defmethod equals ((a structure-object) (b structure-object)
                   &rest keys
                   &key (reference-equality t))
  (declare (ignore keys))
  (if reference-equality
      (eq a b)
      (equalp a b)))

(defmethod equals ((a standard-object) (b standard-object) &rest keys)

  (declare (ignore keys))
  (eq a b))


;;; compound structure comparison is trickier.
;;;
;;; An open question is if the by-key and by-value keys are passed
;;; into the child comparisons. And, should

(defmethod equals ((a hash-table) (b hash-table)
                   &rest keys
                   &key
                     recursive
                     (by-key t)
                     (by-value t)
                     (check-properties t) &allow-other-keys)
  "EQUALS hash-table hash-table &key recursive (by-key t) (by-value t) (check-properties t)

by-key implies checking hash table keys for equality

by-value implies checking hash table values for equality

check-properties implies checking both hash-table-rehash-size and
hash-table rehash-threshhold.

This is expensive: O( 2 * n * (time-of-key-comparison + time-of-value-comparison))."

  ;; Are these the same object?
  (when (eq a b)
    (return-from equals t))

  ;; Do they have different numbers of keys?
  (when (/=  (hash-table-size a) (hash-table-size b))
    (return-from equals nil))

  ;; And now to check.
  (and
   (if by-key
       (loop
          for k1 in (hash-table-keys a)
          for k2 in (hash-table-keys b)
          always (apply
                  #'(lambda (a b)
                      (equals a b :recursive recursive))
                  k1 k2 keys))
       t)
   (if by-value
       (loop
          for v1 in (hash-table-values a)
          for v2 in (hash-table-values b)
          always (apply #'(lambda
                              (a b) (equals a b :recursive recursive))
                        v1 v2 keys))
       t)
   (when check-properties
     (and (= (hash-table-rehash-size a)
             (hash-table-rehash-size b))
          (= (hash-table-rehash-threshold a)
             (hash-table-rehash-threshold b))))))


(defmethod equals ((a array) (b array)
                   &rest keys
                   &key recursive &allow-other-keys)
  "EQUALS array array &key recursive

This implements a straightforward comparison element by element of the array."
  ;; If a more sophisticated array EQUALS is desired, please submit such
  (when (equal (array-dimensions a)
               (array-dimensions b))
    (loop for i from 0 below (array-total-size a)
       always (apply #'(lambda
                           (a b) (equals a b :recursive recursive))
                     (row-major-aref a i)
                     (row-major-aref b i)
                     keys))))


(defgeneric compare (a b &rest keys &key recursive  &allow-other-keys)
  (:documentation "The generic function COMPARE defines methods to
  test the ordering of two objects a and b, if such order exists. The
  result value returned by COMPARE is one of the four symbols: :<, :>,
  :=, or :/=. The COMPARE function returns :/= as result by default; thus
  it can represent partial orders among objects. The equality tests
  should be coherent with what the generic function EQUALS does.

If the argument recursive is T, then COMPARE may recurse down the
`structure` of a and b. The description of each known method contains
the relevant information about its recursive dependent behavior. "))


(defmethod compare ((a t) (b t) &rest keys)
  :/=)

(defmethod compare ((a number) (b number) &rest keys)
  (declare (ignore keys))
  (cond
    ((< a b) :<)
    ((> a b) :>)
    (t :=)))

(defmethod compare ((a symbol) (b symbol) &rest keys)
  (declare (ignore keys))
  (if (eq a b)
      :=
      :/=))

(defmethod compare ((a character) (b character)
                    &rest keys
                    &key case-sensitive)
  (declare (ignore keys))
  (if case-sensitive
      (cond ((string< a b) :<)
            ((string> a b) :>)
            (t :=))
      (cond ((string-lessp a b) :<)
            ((string-greaterp a b) :>)
            (t :=))))

(defmethod compare ((a string) (b string)
                    &rest keys
                    &key (case-sensitive t))
  (declare (ignore keys))
  (if case-sensitive
      (cond ((string< a b) :<)
            ((string> a b) :>)
            (t :=))
      (cond ((string-lessp a b) :<)
            ((string-greaterp a b) :>)
            (t :=))))



(define-condition incomparable-object (error)
  ((value1
    :initarg :value1
    :accessor value1
    :initform nil)
   (value2
    :initarg :value2
    :accessor value2
    :initform nil)))

(defmethod print-object ((object incomparable-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream " - ~a, an ~a, not comparable to~& ~a, an ~a"
            (value1 object)
            (type-of (value1 object))
            (value2 object)
            (type-of (value2 object)))))

(defun incomparable-object (v1 v2)
  (error (make-instance 'incomparable-object
                        :value1 v1
                        :value2 v2 )))

(defgeneric lt (a b &rest keys &key recursive &allow-other-keys))
(defgeneric lte (a b &rest keys &key recursive &allow-other-keys))
(defgeneric gt (a b &rest keys &key recursive &allow-other-keys))
(defgeneric gte (a b &rest keys &key recursive &allow-other-keys))

(let ((underlying #'(lambda (a b &rest keys)
                      (apply #'compare a b keys)) ))
  (defmethod lt (a b &rest keys)
    (case (apply underlying a b keys)
      (:< t)
      (:> nil)
      (:= nil)
      (t
       (incomparable-object a b))))
  (defmethod lte (a b &rest keys)
    (case (apply underlying a b keys)
      (:< t)
      (:> nil)
      (:= t)
      (t
       (incomparable-object a b))))
  (defmethod gt (a b &rest keys)
    (case (apply underlying a b keys)
      (:< nil)
      (:> t)
      (:= nil)
      (t
       (incomparable-object a b))))
  (defmethod gte (a b &rest keys)
    (case (apply underlying a b keys)
      (:< nil)
      (:> t)
      (:= t)
      (t
       (incomparable-object a b)))))

(setf (fdefinition 'lessp) #'lt)
(setf (fdefinition 'not-greaterp) #'lte)

(setf (fdefinition 'greaterp) #'gt)
(setf (fdefinition 'not-lessp) #'gte)



(defgeneric hash-code (a)
  (:documentation "The HASH-CODE generic function is provided as a
companion to EQUALS for the benefit of those Common Lisp
implementations that provide a handle on the inner working of hash
tables (usually in the form of an extra :sxhash or :hash-function
keyword argument to make-hash-table), or for bottom-up hash table
implementations.

HASH-CODE is modeled after the Java hashCode() method of
java.lang.Object. The same description applies almost unchanged.

The general contract of HASH-CODE is the following.

Whenever it is invoked on the same object more than once during an a
Common Lisp session, the HASH-CODE generic function must consistently
return the same fixnum, provided no information used in EQUALS
comparisons on the object a is modified. This integer need not remain
consistent from one Common Lisp session to another.  If two objects
are equal according to the EQUALS generic predicate, then calling the
HASH-CODE generic function on each of the two objects must produce the
same integer result.  It is not required that if two objects are
unequal according to the EQUALS generic predicate, then calling the
HASH-CODE generic function on each of the two objects must produce
distinct integer results. However, the programmer should be aware that
producing distinct integer results for unequal objects may improve the
performance of hashtables.
"))


(defmethod hash-code ((a T))
  "The generic hash-code implementation defaults to
  SXHASH of the value passed in."
  (sxhash a))

;; Register the implementation of cdr-8
(when (boundp '*features*)
  (unless (member :cdr-8 *features*)
    (push :cdr-8 *features*)))
