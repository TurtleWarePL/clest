(in-package #:clest)

(defmacro check-designator (name)
  `(unless (typep ,name 'string)
     (error 'clest:invalid-designator)))

(defmacro check-parent (parent type)
  `(unless (typep ,parent ',type)
     (error 'clest:invalid-parent-type)))

(defclass basic-project (project
                         test-suite-parent-mixin)
  ((test-plans :reader list-test-plans)))

(defmethod make-project ((type (eql 'basic-project)) &key name parent)
  (check-designator name)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defclass basic-test-suite (test-suite
                            test-suite-parent-mixin
                            test-scenario-parent-mixin)
  ())

(defmethod make-test-suite ((type (eql 'basic-test-suite)) &key name parent)
  (check-designator name)
  (check-parent parent test-suite-parent-mixin)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defmethod save-child :before ((parent t) (object basic-test-suite))
  (unless (typep parent 'test-suite-parent-mixin)
    (error 'clest:invalid-parent-type :parent parent :object object)))

(defclass basic-test-scenario (test-scenario
                               test-case-parent-mixin)
  ())

(defmethod make-test-scenario ((type (eql 'basic-test-scenario)) &key name parent)
  (check-designator name)
  (check-parent parent test-scenario-parent-mixin)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defmethod save-child :before ((parent t) (object basic-test-scenario))
  (unless (typep parent 'test-scenario-parent-mixin)
    (error 'clest:invalid-parent-type :parent parent :object object)))

(defclass basic-test-case (test-case
                           tree-leaf-mixin)
  ())

(defmethod make-test-case ((type (eql 'basic-test-case)) &key name parent)
  (check-designator name)
  (check-parent parent test-case-parent-mixin)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defmethod save-child :before ((parent t) (object basic-test-case))
  (unless (typep parent 'test-case-parent-mixin)
    (error 'clest:invalid-parent-type :parent parent :object object)))
