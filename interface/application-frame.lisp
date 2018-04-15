(in-package #:clest)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type foobar ()))

(defgeneric display (frame pane)
  (:method (frame pane)
    (declare (ignore frame))
    (clim:with-output-as-presentation (pane "foobar" 'foobar)
      (format pane "Foobar 1.~%"))
    (format pane "No display method defined.~%")))

(clim:define-application-frame clest (clim:standard-application-frame
                                      project-parent-mixin)
  ((tree-type :initarg :tree))
  (:geometry :width 800 :height 1600)
  (:pane (clim:make-pane (slot-value clim:*application-frame* 'tree-type)
                         :root clim:*application-frame*))
  (:menu-bar nil))

(defun run-ui (&optional (tree 'simplistic-tree-gadget))
  (setf *projects* (clim:make-application-frame 'clest :tree tree))
  (clest/unit-tests::populate-data 'basic-project
                                   'basic-test-suite
                                   'basic-test-scenario
                                   *projects*)
  (clim:run-frame-top-level *projects*))
