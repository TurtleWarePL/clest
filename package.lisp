;;;; package.lisp

(defpackage #:clest
  (:use #:cl #:alexandria)
  ;; Protocol
  (:export #+(or)classes
           #:project
           #:test-suite
           #:test-scenario
           #:test-case
           ;; conditions
           #:clest-error
           #:child-already-exists
           #:child-doesnt-exist
           #:invalid-designator
           #:invalid-parent-type
           ;; project protocol
           #:make-project
           #:extensions
           ;; test suite protocol
           #:make-test-suite
           ;; test scenario protocol
           #:make-test-scenario
           ;; test case protocol
           #:make-test-case
           ;; synopsis protocol
           #:name
           #:description
           ;; test tree protocol
           #:list-children
           #:save-child
           #:load-child
           #:delete-child
           ;; #:testing-plan  #:make-testing-plan
           ;; #:build         #:make-build
           )
  ;; Default implementation
  (:export #:basic-project
           #:basic-test-suite
           #:basic-test-scenario))
