(in-package #:clest)

(defclass basic-project (project
                         project-parent-mixin
                         test-suite-parent-mixin)
  ((test-plans :reader list-test-plans)))

(defmethod make-project ((type (eql 'basic-project)) &key name parent)
  (check-type name string)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defclass basic-test-suite (test-suite
                            test-suite-parent-mixin
                            test-scenario-parent-mixin)
  ())

(defmethod make-test-suite ((type (eql 'basic-test-suite)) &key name parent)
  (check-type name string)
  (check-type parent (or test-suite project))
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defclass basic-test-scenario (test-scenario
                               test-case-parent-mixin)
  ())

(defmethod make-test-scenario ((type (eql 'basic-test-scenario)) &key name parent)
  (check-type name string)
  (check-type parent (or test-suite project))
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

(defclass basic-test-case (test-case
                           tree-leaf-mixin)
  ())

(defmethod make-test-case ((type (eql 'basic-test-case)) &key name parent)
  (check-type name string)
  (check-type parent test-scenario)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))

