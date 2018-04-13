;;;; clest.lisp

(in-package #:clest)

(defmacro define-protocol-class (name &optional super &rest args)
  `(progn
     (defclass ,name ,super () ,@args)
     (defgeneric ,(alexandria:symbolicate 'make- name)
         (type &rest initargs &key name &allow-other-keys)
       (:method ((type (eql ',name)) &key name)
         (error "Protocol class ~s can't be instantiated." ',name)))))

(defmacro define-protocol-function (name (&rest args) &body body)
  (declare (ignore body))
  `(defgeneric ,name (,@args)))

(defmacro define-implementation (names (&rest args) &body body)
  `(progn
     ,@(mapcar #'(lambda (name)
                   `(defmethod ,name (,@args) ,@body))
               (alexandria:ensure-list names))))

(defmacro define-tree-protocol (name super (list save find delete) &rest args)
  (declare (ignore name super args))
  `(progn (defgeneric ,list (parent))
          (defgeneric ,save (parent object))
          (defgeneric ,find (parent name))
          (defgeneric ,delete (parent name))))

(defmacro define-tree-implementation ((parent-class child-class)
                                      (list save find delete))
  `(progn (define-implementation ,list ((parent ,parent-class))
            (list-children parent))
          (define-implementation ,save ((parent ,parent-class) (object ,child-class))
            (save-child parent object))
          (define-implementation ,find ((parent ,parent-class) (name string))
            (load-child parent name))
          (define-implementation ,delete ((parent ,parent-class) (name string))
            (delete-child parent name))))


;;; Mixins
(defclass name-desc-mixin ()
  ((name :initform (error "Name is mandatory for this object.")
         :initarg :name
         :type 'string
         :accessor name)
   (desc :initform nil
         :initarg :description
         :type 'string
         :accessor description)))

(defclass tree-parent-mixin (name-desc-mixin)
  ((children :initform (make-hash-table :test #'equal)
             :initarg :children
             :type 'hash-table
             :accessor %children)))

(defclass project-parent-mixin (tree-parent-mixin) ())

(define-tree-protocol tree ()
  (list-children save-child load-child delete-child))

(defmethod list-children ((object tree-parent-mixin))
  (alexandria:hash-table-values (%children object)))

(defmethod save-child ((parent tree-parent-mixin) object)
  (if #1=(gethash (name object) (%children parent))
      (error "Child ~s already exist in ~s." (name object) (name parent))
      (setf #1# object)))

(defmethod load-child ((parent tree-parent-mixin) (name string))
  (or (gethash name (%children parent))
      (error "Child ~s doesn't exist in ~s." name (name parent))))

(defmethod delete-child ((parent tree-parent-mixin) (name string))
  (or (remhash name (%children parent))
      (error "Child ~s doesn't exist in ~s." name (name parent))))
