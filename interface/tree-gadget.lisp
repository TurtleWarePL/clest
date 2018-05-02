(in-package #:clest)

(defclass abstract-tree-gadget (clim:basic-gadget)
  ((root :initarg :root
         :initform (error ":root argument is obligatory.")
         :reader tree-root)))

(defclass generic-tree-gadget (abstract-tree-gadget climi::always-repaint-background-mixin)
  ((expanded :initform (make-hash-table) :reader expanded)
   (buttons :initform (make-instance 'clim:standard-sequence-output-record)
            :accessor buttons)
   (max-x :initform 250)
   (max-y :initform 60))
  (:default-initargs :text-style (clim:make-text-style :fixed nil :normal)))

(defclass node-output-record (climi::basic-output-record)
  ((node :initarg :node :reader node)))

(defun toggle-node (gadget node)
  (setf (gethash node (expanded gadget)) (not (gethash node (expanded gadget)))))

(defun expandedp (gadget node)
  (gethash node (expanded gadget)))

(defmethod initialize-instance :after ((record node-output-record)
                                       &key x1 y1 x2 y2)
  (setf (clim:rectangle-edges* record)
        (values x1 y1 x2 y2)))

(defmethod clim:compose-space ((pane generic-tree-gadget) &key width height)
  (declare (ignorable width height))
  (with-slots (max-x max-y) pane
    (clim:make-space-requirement :min-width max-x :min-height max-y
                                 :max-width max-x :max-height max-y
                                 :width max-x :height max-y)))

(defmethod clim:handle-repaint ((gadget generic-tree-gadget) region)
  (declare (ignore region))
  (clim:clear-output-record (buttons gadget))
  (let ((current-y 0)
        (current-x 0))
    (setf (slot-value gadget 'max-x) 220
          (slot-value gadget 'max-x) 50)
    (labels ((present-node (node)
               (let ((text (if (clest::list-children node)
                               (if (expandedp gadget node)
                                   (format nil " - ~A" (clest::name node))
                                   (format nil " + ~A" (clest::name node)))
                               (format nil "   ~A" (clest::name node)))))
                 (clim:draw-text* gadget text current-x current-y :align-y :top)
                 (multiple-value-bind (text-width text-height) (clim:text-size gadget "   ")
                   (let ((x1 current-x)
                         (y1 current-y)
                         (x2 (+ current-x text-width))
                         (y2 (+ current-y text-height)))
                     (clim:draw-rectangle* gadget x1 y1 x2 y2 :filled nil :ink clim:+darkred+)
                     (clim:add-output-record
                      (make-instance 'node-output-record
                                     :node node :x1 x1 :y1 y1 :x2 x2 :y2 y2)
                      (buttons gadget))))
                 (multiple-value-bind (text-width text-height)
                     (clim:text-size gadget text)
                   (incf current-y text-height)
                   (maxf (slot-value gadget 'max-y) current-y)
                   (maxf (slot-value gadget 'max-x) (+ current-x text-width)))
                 (when (expandedp gadget node)
                   (incf current-x 20)
                   (dolist (c (clest:list-children node))
                     (present-node c))
                   (decf current-x 20)))))
      (dolist (c (clest:list-children (tree-root gadget)))
        (present-node c))))
  (clim:change-space-requirements gadget))

(defmethod clim:handle-event ((gadget generic-tree-gadget)
                              (event clim:pointer-button-press-event))
  (clim:map-over-output-records-containing-position
   #'(lambda (record)
       (toggle-node gadget (node record))
       (clim:handle-repaint gadget clim:+everywhere+)
       (return-from clim:handle-event nil))
   (buttons gadget)
   (clim:pointer-event-x event)
   (clim:pointer-event-y event)))
