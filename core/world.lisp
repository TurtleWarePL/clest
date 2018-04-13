(in-package #:clest)

;;; Singleton project list (parent is nil)
(defvar *projects* (make-hash-table :test #'equal))

(defmethod list-children ((parent null))
  (alexandria:hash-table-values *projects*))

(defmethod load-child ((parent null) (name string))
  (or (gethash name *projects*)
      (error "Project ~s doesn't exist." name)))

(defmethod delete-child ((parent null) (name string))
  (or (remhash name *projects*)
      (error "Project ~s doesn't exist." name)))

(defmethod save-child ((parent null) (object project))
  (if (null #1=(gethash (name object) *projects*))
      (setf #1# object)
      (error "Project ~s already exist." (name object))))


;;; Parent mixins
(defclass project-parent-mixin       (tree-parent-mixin) ())
(defclass test-suite-parent-mixin    (tree-parent-mixin) ())
(defclass test-scenario-parent-mixin (tree-parent-mixin) ())
(defclass test-case-parent-mixin     (tree-parent-mixin) ())
