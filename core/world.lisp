(in-package #:clest)

;;; Singleton project list (parent is nil)
(defvar *projects* (make-hash-table :test #'equal))

(define-implementation (list-projects list-children) ((parent null))
  (alexandria:hash-table-values *projects*))

(define-implementation (find-project load-project) ((parent null) (name string))
  (or (gethash name *projects*)
      (error "Project ~s doesn't exist." name)))

(define-implementation (delete-project delete-child) ((parent null) (name string))
  (or (remhash name *projects*)
      (error "Project ~s doesn't exist." name)))

(define-implementation (save-project save-child) ((parent null) (object project))
  (if (null #1=(gethash (name object) *projects*))
      (setf #1# object)
      (error "Project ~s already exist." (name object))))


;;; Parent mixins
(defclass project-parent-mixin       (tree-parent-mixin) ())
(defclass test-suite-parent-mixin    (tree-parent-mixin) ())
(defclass test-scenario-parent-mixin (tree-parent-mixin) ())
(defclass test-case-parent-mixin     (tree-parent-mixin) ())

(define-tree-implementation (project-parent-mixin project)
    (list-projects save-project load-project delete-project))

(define-tree-implementation (test-suite-parent-mixin test-suite)
    (list-test-suites save-test-suite load-test-suite delete-test-suite))

(define-tree-implementation (test-scenario-parent-mixin test-scenario)
    (list-test-scenarios save-test-scenario load-test-scenario delete-test-scenario))

(define-tree-implementation (test-case-parent-mixin test-case)
    (list-test-cases save-test-case load-test-case delete-test-case))
