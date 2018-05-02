(in-package #:clest)

;;; Singleton project list (parent is nil)
(defvar *projects* (make-hash-table :test #'equal))

(defmethod list-children ((parent null))
  (hash-table-values *projects*))

(defmethod load-child ((parent null) (name string))
  (or (gethash name *projects*)
      (error 'clest:child-doesnt-exist :parent parent :name name)))

(defmethod delete-child ((parent null) (name string))
  (or (remhash name *projects*)
      (error 'clest:child-doesnt-exist :parent parent :name name)))

(defmethod save-child ((parent null) (object project))
  (if-let ((child #1=(gethash (name object) *projects*)))
    (error 'clest:child-already-exists :parent parent :child child :object object)
    (setf #1# object)))


;;; Parent mixins
(defclass project-parent-mixin       (tree-parent-mixin) ())
(defclass test-suite-parent-mixin    (tree-parent-mixin) ())
(defclass test-scenario-parent-mixin (tree-parent-mixin) ())
(defclass test-case-parent-mixin     (tree-parent-mixin) ())
