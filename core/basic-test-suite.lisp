(in-package #:clest)

(defclass basic-test-suite (test-suite
                            test-suite-parent-mixin
                            test-scenario-parent-mixin)
  ())

(defmethod make-test-suite ((type (eql 'basic-test-suite)) &key name parent)
  (check-type name string)
  (let ((object (make-instance type :name name)))
    (save-test-suite parent object)))
