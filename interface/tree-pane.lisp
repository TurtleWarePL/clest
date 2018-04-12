(in-package #:clest)

(defclass abstract-tree-pane (clim:clim-stream-pane)
  ((root :initarg :root
         :initform (error ":root argument is obligatory."))
   (toggled :initform nil)))

(defun toggle-node (tree node)
  (check-type tree abstract-tree-pane)
  (check-type node tree-parent-mixin)
  (with-slots (toggled) tree
    (if (member node toggled)
        (setf toggled (delete node toggled))
        (push node toggled))))

(defclass simplistic-tree-pane (abstract-tree-pane) ()
  (:default-initargs :text-family :fixed))

(defmethod clim:handle-repaint ((pane simplistic-tree-pane) region)
  (declare (ignore region))
  (clim:draw-rectangle* pane 100 100 150 200)
  (let* ((text-style (clim:pane-text-style pane))
         (text-height (+ (clim:text-style-descent text-style pane)
                         (clim:text-style-ascent text-style pane)))
         (current-y 0))
    (labels ((print-node (branch)
               (dolist (c (list-children branch))
                 (clim:with-output-as-presentation (pane c 'tree-parent-mixin)
                   (clim:draw-text* pane
                                    (format nil "+ ~A" (name c))
                                    0
                                    (incf current-y text-height)))
                 ;(format pane "+ ~A~%" (name c))
                 ;; (if (member c (slot-value pane 'toggled))
                 ;;     (progn (format pane "- ~A~%" (name c))
                 ;;            (incf (climi::stream-text-left-margin pane) 15)
                 ;;            (unwind-protect (print-node c)
                 ;;              (decf (climi::stream-text-left-margin pane) 15)))
                 ;;     (format pane "+ ~A~%" (name c)))
                 )))
      (print-node (slot-value pane 'root)))))

(defclass material-tree-pane (abstract-tree-pane) ())
