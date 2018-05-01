;;;; package.lisp

(defpackage #:clest
  (:use #:cl)
  ;; Protocol
  (:export #:project
           #:test-suite
           #:test-scenario
           #:test-case
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
