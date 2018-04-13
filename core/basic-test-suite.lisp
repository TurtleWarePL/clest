(in-package #:clest)

(defclass basic-test-suite (test-suite
                            test-suite-parent-mixin
                            test-scenario-parent-mixin)
  ())

(defmethod make-test-suite ((type (eql 'basic-test-suite)) &key name parent)
  (check-type name string)
  (check-type parent (or test-suite project))
  (let ((object (make-instance type :name name)))
    (save-child parent object)))
