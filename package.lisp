;;;; package.lisp

(defpackage #:clest
  (:use #:cl)
  ;; Protocol
  (:export #:project       #:make-project       #:extensions
           #:test-suite    #:make-test-suite
           #:test-scenario #:make-test-scenario
           #:test-case     #:make-test-case
           ;; #:testing-plan  #:make-testing-plan
           ;; #:build         #:make-build
           ;; test suite tree protocol
           #:list-children #:load-child #:delete-child
           #:save-project       ;; project parent protocols
           #:save-test-suite    ;; test-suite parent protocol
           #:save-test-scenario ;; test-scenario parent protocol
           )
  ;; Default implementation
  (:export #:basic-project
           #:basic-test-suite
           #:basic-test-scenario))
