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
  (:menu-bar nil)
  )

(defmethod clim:run-frame-top-level :before ((frame clest) &key)
  ;; (setf (clim-extensions:list-pane-items (clim:find-pane-named frame 'projects))
  ;;       (list-projects frame))
  )

;; (define-clest-command (get-foobar :name t :menu t) ((xxx foobar))
;;   (format *debug-io* "foobar is ~s~%" xxx))

(defun run-ui (&optional (tree 'simplistic-tree-gadget))
  (setf *projects* (clim:make-application-frame 'clest :tree tree))
  (clest/unit-tests::populate-data 'basic-project
                                   'basic-test-suite
                                   'basic-test-scenario
                                   *projects*)
  (clim:run-frame-top-level *projects*))
