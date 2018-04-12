(defpackage #:clest/unit-tests
  (:use #:cl #:1am #:clest)
  (:export #:run-tests))
(in-package #:clest/unit-tests)

(defun run-tests (&optional
                    (project-class 'clest:basic-project)
                    (test-suite-class 'clest:basic-test-suite)
                    (test-scenario-class 'clest:basic-test-scenario))
  (let ((clest::*projects* (make-hash-table :test #'equal)))
    (populate-data project-class test-suite-class test-scenario-class)
    (test-project-protocol project-class)
    (terpri)
    (test-test-suite-protocol test-suite-class :parent (clest:load-project nil "Sandbox Project"))
    (terpri)
    clest::*projects*))

(defun populate-data (project-class test-suite-class test-scenario-class &optional parent)
  (let ((parent (clest:make-project project-class :name "Instinct Engine" :parent parent)))
    (clest:make-test-suite test-suite-class :parent parent :name "Deployment and Migration")
    (clest:make-test-suite test-suite-class :parent parent :name "Machine Imagination")
    (clest:make-test-suite test-suite-class :parent parent :name "Documentation")
    (clest:make-test-suite test-suite-class :parent parent :name "User Interface")
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Backend")))
      (clest:make-test-suite test-suite-class :parent parent :name "REST API")
      (clest:make-test-suite test-suite-class :parent parent :name "Unit Tests")
      (clest:make-test-suite test-suite-class :parent parent :name "Query language"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Reports")))
      (clest:make-test-scenario test-scenario-class :parent parent :name "Make report")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Remove report"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Notifications")))
      (clest:make-test-suite test-suite-class :parent parent :name "Email")
      (clest:make-test-suite test-suite-class :parent parent :name "Dashboard")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Event createion")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Event subscription")))
  (let ((parent (clest:make-project project-class :name "McCLIM" :parent parent)))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Output Records")))
      (clest:make-test-suite test-suite-class :parent parent :name "Composite")
      (clest:make-test-suite test-suite-class :parent parent :name "Display")
      (clest:make-test-suite test-suite-class :parent parent :name "Logical"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Sheets")))
      (clest:make-test-suite test-suite-class :parent parent :name "Panes")
      (clest:make-test-suite test-suite-class :parent parent :name "Gadgets")
      (clest:make-test-suite test-suite-class :parent parent :name "Streams"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Presentations")))
      (clest:make-test-suite test-suite-class :parent parent :name "Methods")
      (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Application")))
        (clest:make-test-suite test-suite-class :parent parent :name "Input")
        (clest:make-test-suite test-suite-class :parent parent :name "Dragging"))
      (clest:make-test-suite test-suite-class :parent parent :name "Translators")))
  (clest:make-project project-class :name "Sandbox Project"        :parent parent)
  (let ((parent (clest:make-project project-class :name "Embeddable Common-Lisp" :parent parent)))
    (clest:make-test-suite test-suite-class :parent parent :name "Deployment")
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Compilation")))
      (clest:make-test-suite test-suite-class :parent parent :name "Bytecodes compiler")
      (clest:make-test-suite test-suite-class :parent parent :name "Native compiler")
      (clest:make-test-suite test-suite-class :parent parent :name "Environments"))))

(defun test-project-protocol (project-class &key parent
                              &aux (len (length (clest:list-projects parent))))
  ;; only strings as project names
  (signals error
    (clest:make-project project-class :name 3 :parent parent))
  (signals error
    (clest:make-project project-class :name :xxx :parent parent))
  (clest:make-project project-class :name "Foobar" :parent parent)
  (is (= (1+ len) (length (clest:list-projects parent))))
  ;; can't load non-existing project
  (signals error (clest:load-project parent "Quxbar"))
  (clest:load-project parent "Foobar")
  ;; delete project (but only once)
  (clest:delete-project parent "Foobar")
  (signals error (clest:delete-project parent "Foobar"))
  (is (= len (length (list-projects parent)))))

(defun test-test-suite-protocol (test-suite-class &key parent)
  ;; Adding new test suites
  (is (null (clest:list-test-suites parent)))
  (let ((ts1 (clest:make-test-suite test-suite-class :name "TestSuite1" :parent parent))
        (ts2 (clest:make-test-suite test-suite-class :name "TestSuite2" :parent parent)))
   (signals error
     (clest:make-test-suite test-suite-class :name :test-suite-3 :parent parent))
   (signals error
     (clest:make-test-suite test-suite-class :name "TestSuite1" :parent parent))
   (is (= (length (clest:list-test-suites parent)) 2))

   ;; Adding sub-test suites
   (clest:make-test-suite test-suite-class :name "TestSuite1" :parent ts1)
   (clest:make-test-suite test-suite-class :name "TestSuite2a" :parent ts2)
   (is (= (length (clest:list-test-suites parent)) 2))
   (is (= (length (clest:list-test-suites ts1)) 1))
   (is (= (length (clest:list-test-suites ts2)) 1))

   ;; Removing test-suites
   (clest:delete-test-suite ts1 "TestSuite1")
   (clest:delete-test-suite parent "TestSuite2")
   (is (= (length (clest:list-test-suites parent)) 1))
   (is (= (length (clest:list-test-suites ts1)) 0))
   (is (= (length (clest:list-test-suites ts2)) 1))))

(defun test-test-scenario-protocol (test-scenario-class &key parent)
  ;; Adding new test suites
  (is (null (clest:list-test-scenarios parent)))
  (let ((ts1 (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent parent))
        (ts2 (clest:make-test-scenario test-scenario-class :name "TestScenario2" :parent parent)))
    (signals error
      (clest:make-test-scenario test-scenario-class :name :test-suite-3 :parent parent))
    (signals error
      (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent parent))
    (is (= (length (clest:list-test-scenarios parent)) 2))

    ;; Adding sub-test suites
    (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent ts1)
    (clest:make-test-suite test-scenario-class :name "TestScenario2a" :parent ts2)
    (is (= (length (clest:list-test-scenarios parent)) 2))
    (is (= (length (clest:list-test-scenarios ts1)) 1))
    (is (= (length (clest:list-test-scenarios ts2)) 1))

    ;; Removing test-suites
    (clest:delete-test-suite ts1 "TestSuite1")
    (clest:delete-test-suite parent "TestSuite2")
    (is (= (length (clest:list-test-suites parent)) 1))
    (is (= (length (clest:list-test-suites ts1)) 0))
    (is (= (length (clest:list-test-suites ts2)) 1))))
