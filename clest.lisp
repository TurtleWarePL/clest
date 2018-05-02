;;;; clest.lisp

(in-package #:clest)

(defmacro define-protocol-class (name &optional super &rest args)
  `(progn
     (defclass ,name ,super () ,@args)
     (defgeneric ,(symbolicate 'make- name)
         (type &rest initargs &key name &allow-other-keys)
       (:method ((type (eql ',name)) &key name)
         (declare (ignore type name))
         (error "Protocol class ~s can't be instantiated." ',name)))))


;;; Mixins
(defclass name-desc-mixin ()
  ((name :initform (error "Name is mandatory for this object.")
         :initarg :name
         :type 'string
         :accessor name)
   (desc :initform nil
         :initarg :description
         :accessor description)))

(defclass tree-parent-mixin (name-desc-mixin)
  ((children :initform (make-hash-table :test #'equal)
             :initarg :children
             :type 'hash-table
             :accessor %children)))

(defclass tree-leaf-mixin (name-desc-mixin) ())

(defclass project-parent-mixin (tree-parent-mixin) ())

(defgeneric list-children (parent))
(defgeneric save-child (parent object))
(defgeneric load-child (parent name))
(defgeneric delete-child (parent name))

(defmethod list-children ((object tree-parent-mixin))
  (hash-table-values (%children object)))

(defmethod list-children ((object tree-leaf-mixin))
  nil)

(defmethod list-children ((object hash-table))
  (hash-table-values object))

(defmethod save-child ((parent tree-parent-mixin) object)
  (if-let ((child #1=(gethash (name object) (%children parent))))
    (error 'clest:child-already-exists :parent parent :child child :object object)
    (setf #1# object)))

(defmethod load-child ((parent tree-parent-mixin) (name string))
  (or (gethash name (%children parent) )
      (error 'clest:child-doesnt-exist :parent parent :name  name)))

(defmethod delete-child ((parent tree-parent-mixin) (name string))
  (or (remhash name (%children parent))
      (error 'clest:child-doesnt-exist :parent parent :name  name)))
