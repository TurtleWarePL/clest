;;;; package.lisp

(defpackage #:clest
  (:use #:cl)
  ;; Protocol
  (:export #:project       #:make-project
           #:test-suite    #:make-test-suite
           #:test-scenario #:make-test-scenario
           #:test-case     #:make-test-case
           #:testing-plan  #:make-testing-plan
           #:release       #:make-release
           ;; project protocol
           #:list-projects
           #:save-project
           #:load-project
           #:delete-project
           ;; test-suite protocol
           #:list-test-suites
           #:save-test-suite
           #:load-test-suite
           #:delete-test-suite
           ;; test-scenario protocol
           #:list-test-scenarios
           #:save-test-scenario
           #:load-test-scenario
           #:delete-test-scenario)
  ;; Default implementation
  (:export #:basic-project
           #:basic-test-suite
           #:basic-test-scenario))
