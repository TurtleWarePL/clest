;;; protocol.lisp

(in-package #:clest)

;;;; Classes
(define-protocol-class project)       ; root for all project suites
(define-protocol-class test-suite)    ; test suites - intermediate tree nodes
(define-protocol-class test-scenario) ; usage pattern (tree leafs)
(define-protocol-class test-case)     ; cases of usage pattern
(define-protocol-class testing-plan)  ; testing plan
(define-protocol-class release)       ; currently tested build (release)


;;;; Protocol

;;; Top-level entity
(define-tree-protocol project-parent ()
  (list-projects
   save-project
   load-project
   delete-project))

;;; Tests
(define-tree-protocol test-suite-parent ()
  (list-test-suites
   save-test-suite
   load-test-suite
   delete-test-suite))

(define-tree-protocol test-scenario-parent ()
  (list-test-scenarios
   save-test-scenario
   load-test-scenario
   delete-test-scenario))

(define-tree-protocol test-case-parent ()
  (list-test-cases
   save-test-case
   load-test-case
   delete-test-case))


;; (define-tree-protocol test-step-parent ()
;;   (list-test-steps
;;    insert-test-step
;;    load-test-step
;;    delete-test-step))

;;; Planning
(define-tree-protocol test-plan (tree)
  (list-plans
   save-plan
   load-plan
   delete-plan))

(define-tree-protocol release (tree)
  (list-releases
   save-release
   load-release
   delete-release))
