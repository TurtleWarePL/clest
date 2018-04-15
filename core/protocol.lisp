;;;; protocol.lisp

(in-package #:clest)

;;; Classes
(define-protocol-class project)       ; root for all project suites
(define-protocol-class test-suite)    ; test suites - intermediate tree nodes
(define-protocol-class test-scenario) ; usage pattern (tree leafs)
(define-protocol-class test-case)     ; cases of usage pattern
#|
(define-protocol-class testing-plan)  ; testing plan
(define-protocol-class build)         ; currently tested build (release)
(define-protocol-class requirement)   ; software specification
(define-protocol-class documentation) ; software documentation
|#


;;; Conditions
(define-condition clest-error (error) ())
(define-condition child-already-exists (clest-error) ())
(define-condition child-doesnt-exist (clest-error) ())
(define-condition invalid-designator (clest-error) ())
;(define-condition invalid-parent-type (clest-error) )


;;; Synopsis protocol
(defgeneric name (object)
  (:documentation "Returns unique object string identifier."))

(defgeneric description (object)
  (:documentation "Returns opaque object being object's description."))

;;; Test suite tree protocol
(defgeneric list-children (parent))
(defgeneric save-child (parent object))
(defgeneric load-child (parent object))
(defgeneric delete-child (parent object))

;;; Project protocol
(defgeneric make-project (type &key name parent &allow-other-keys))
(defgeneric extensions (project))

;;; Test suite protocol
(defgeneric make-test-suite (type &key name parent &allow-other-keys))

;;; Test scenario protocol
(defgeneric make-test-scenario (type &key name parent &allow-other-keys))
(defgeneric promote-to-test-suite (type object))

;;; Test case protocol
(defgeneric make-test-case (type &key name parent &allow-other-keys))
(defgeneric promote-to-test-scenario (type object))
