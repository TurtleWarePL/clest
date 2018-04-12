(in-package #:clest)

(defclass basic-test-scenario (test-scenario
                               test-case-parent-mixin)
  ())

(defmethod make-test-scenario ((type (eql 'basic-test-scenario)) &key name parent)
  (check-type name string)
  (let ((object (make-instance type :name name)))
    (save-test-scenario parent object)))
