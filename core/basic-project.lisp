(in-package #:clest)

(defclass basic-project (project
                         project-parent-mixin
                         test-suite-parent-mixin)
  ((test-plans :reader list-test-plans)))

(defmethod make-project ((type (eql 'basic-project)) &key name parent)
  (check-type name string)
  (let ((object (make-instance type :name name)))
    (save-child parent object)))
