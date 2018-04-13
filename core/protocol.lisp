;;; protocol.lisp

(in-package #:clest)

;;;; Classes
(define-protocol-class project)       ; root for all project suites
(define-protocol-class test-suite)    ; test suites - intermediate tree nodes
(define-protocol-class test-scenario) ; usage pattern (tree leafs)
(define-protocol-class test-case)     ; cases of usage pattern
;(define-protocol-class testing-plan)  ; testing plan
;(define-protocol-class build)         ; currently tested build (release)
;(define-protocol-class requirement)   ; software specification
;(define-protocol-class documentation) ; software documentation


;;;; Protocol

