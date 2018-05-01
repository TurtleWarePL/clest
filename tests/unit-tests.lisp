(defpackage #:clest/unit-tests
  (:use #:cl #:1am)
  (:export #:run-tests))
(in-package #:clest/unit-tests)

(defun run-tests (&optional
                    (project-class 'clest:basic-project)
                    (test-suite-class 'clest:basic-test-suite)
                    (test-scenario-class 'clest:basic-test-scenario)
                    (test-case-class 'clest::basic-test-case))
  (let ((clest::*projects* (make-hash-table :test #'equal)))
    (populate-data project-class test-suite-class test-scenario-class test-case-class)
    (test-project-protocol project-class)
    (terpri)
    (test-test-suite-protocol test-suite-class :parent (clest:load-child nil "Sandbox Project"))
    (terpri)
    (test-test-scenario-protocol test-scenario-class :parent (clest:load-child nil "Sandbox Project"))
    (terpri)
    ;; (test-test-case-protocol test-case-class :parent (clest:load-child nil "Sandbox Project"))
    ;; (terpri)
    clest::*projects*))

(defun populate-data (project-class test-suite-class test-scenario-class test-case-class
                      &optional parent)
  (let ((parent (clest:make-project project-class :name "Common Lisp Tests Go East" :parent parent)))
    (clest:make-test-suite test-suite-class :parent parent :name "Building and Deployment")
    (clest:make-test-suite test-suite-class :parent parent :name "Test suite managament")
    (clest:make-test-suite test-suite-class :parent parent :name "Testing plan managament")
    (clest:make-test-suite test-suite-class :parent parent :name "User Interface")
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Backend")))
      (clest:make-test-suite test-suite-class :parent parent :name "REST API")
      (clest:make-test-suite test-suite-class :parent parent :name "Unit Tests"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Reports")))
      (clest:make-test-scenario test-scenario-class :parent parent :name "Make report")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Remove report"))
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Documentation")))
      (clest:make-test-suite test-suite-class :parent parent :name "User manual")
      (clest:make-test-suite test-suite-class :parent parent :name "Requirement managament")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Ensure completeness")
      (clest:make-test-scenario test-scenario-class :parent parent :name "Ensure correctness")))
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
  (clest:make-project project-class :name "Sandbox Project" :parent parent)
  (let ((parent (clest:make-project project-class :name "Embeddable Common-Lisp" :parent parent)))
    (clest:make-test-suite test-suite-class :parent parent :name "Deployment")
    (let ((parent (clest:make-test-suite test-suite-class :parent parent :name "Compilation")))
      (clest:make-test-suite test-suite-class :parent parent :name "Bytecodes compiler")
      (clest:make-test-suite test-suite-class :parent parent :name "Native compiler")
      (clest:make-test-suite test-suite-class :parent parent :name "Environments")))
  (let ((parent (clest:make-project project-class :name "Test Project X" :parent parent)))
    (dotimes (v 3)
      (let ((parent (clest:make-test-suite test-suite-class
                                          :parent parent
                                          :name (format nil "Test Suite ~s" (1+ v)))))
        (dotimes (v 2)
          (let ((parent (clest:make-test-suite test-suite-class
                                              :parent parent
                                              :name (format nil "(Sub-)Test Suite ~s" (1+ v)))))
            #1=(dotimes (v 2)
                 (let ((parent (clest:make-test-scenario
                                test-scenario-class
                                :parent parent
                                :name (format nil "Test Scenario ~s" (1+ v)))))
                   (dotimes (v 3)
                     (clest:make-test-case
                      test-case-class
                      :parent parent
                      :name (format nil "Test Case ~s" (1+ v))))))))
        #1#))))

(defun test-project-protocol (project-class &key parent
                              &aux (len (length (clest:list-children parent))))
  ;; only strings as project names
  (signals error
    (clest:make-project project-class :name 3 :parent parent))
  (signals error
    (clest:make-project project-class :name :xxx :parent parent))
  (clest:make-project project-class :name "Foobar" :parent parent)
  (is (= (1+ len) (length (clest:list-children parent))))
  ;; can't load non-existing project
  (signals error (clest:load-child parent "Quxbar"))
  (clest:load-child parent "Foobar")
  ;; delete project (but only once)
  (clest:delete-child parent "Foobar")
  (signals error (clest:delete-child parent "Foobar"))
  (is (= len (length (clest:list-children parent)))))

(defun test-test-suite-protocol (test-suite-class &key parent)
  ;; Adding new test suites
  (is (null (clest:list-children parent)))
  (let ((ts1 (clest:make-test-suite test-suite-class :name "TestSuite1" :parent parent))
        (ts2 (clest:make-test-suite test-suite-class :name "TestSuite2" :parent parent)))
   (signals error
     (clest:make-test-suite test-suite-class :name :test-suite-3 :parent parent))
   (signals error
     (clest:make-test-suite test-suite-class :name "TestSuite1" :parent parent))
   (is (= (length (clest:list-children parent)) 2))

   ;; Adding sub-test suites
   (clest:make-test-suite test-suite-class :name "TestSuite1" :parent ts1)
   (clest:make-test-suite test-suite-class :name "TestSuite2a" :parent ts2)
   (is (= (length (clest:list-children parent)) 2))
   (is (= (length (clest:list-children ts1)) 1))
   (is (= (length (clest:list-children ts2)) 1))

   ;; Removing test-suites
   (clest:delete-child ts1 "TestSuite1")
   (clest:delete-child parent "TestSuite2")
   (is (= (length (clest:list-children parent)) 1))
   (is (= (length (clest:list-children ts1)) 0))
   (is (= (length (clest:list-children ts2)) 1))))

(defun test-test-scenario-protocol (test-scenario-class &key parent)
  ;; Adding new test scenarios
  (let ((no (length (clest:list-children parent)))
        (ts1 (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent parent))
        (ts2 (clest:make-test-scenario test-scenario-class :name "TestScenario2" :parent parent)))
    (signals error
      (clest:make-test-scenario test-scenario-class :name :test-suite-3 :parent parent))
    (signals error
      (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent parent))
    (is (= (length (clest:list-children parent)) (+ no 2)))

    ;; Adding sub-test suites (should error)
    (signals error
      (clest:make-test-scenario test-scenario-class :name "TestScenario1" :parent ts1))
    (signals error
      (clest:make-test-suite test-scenario-class :name "TestScenario2a" :parent ts2))))

(defun test-test-case-protocol (test-case-class &key parent)
  (clest:make-test-case test-case-class :parent parent :name "TestCase1")
  (clest:make-test-case test-case-class :parent parent :name "TestCase2")
  (is (= (length (clest:list-children parent)) 2)))
