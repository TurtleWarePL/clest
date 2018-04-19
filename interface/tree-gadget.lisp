(in-package #:clest)

#|

How to define simplistic gadget from scratch
--------------------------------------------

All gadgets must obey gadget protocol and be subclasses of a protocol class
CLIM:GADGET (usually CLIM:BASIC-GADGET). Gadget implementation has three
stages (the last one is optional).

1. Define the abstract gadget which implements the logical functionality and
then its realization.

2. Create a generic gadget implementation which conforms to generic McCLIM "look
and feel" using only CLIM infrastructure.

3. Implement frame manager -specific versions of the gadget. It may be a glue
between CLIM abstract gadget and the native widget object or it may be
constructed by other means.

Usually programmer specifies abstract gadget he is interested in and
frame-manager picks the best implementation it has (specialized for toolkit and
if it doesn't exist – generic one).

Keep in mind that gadgets doesn't blend well with presentations and stream pane,
they are not meant to be used together. It is best to look at them as separate
entities which input are events and output is their canvas (and side effects
which are gadget-specific).


Abstract gadget
===============

|#
(defclass abstract-tree-gadget (clim:basic-gadget)
  ((root :initarg :root
         :initform (error ":root argument is obligatory."))))
#|

Simplistic gadget (version 1)
=============================

Implementing a gadget is an easy task. We define a class and specialize
CLIM:HANDLE-REPAINT and CLIM:COMPOSE-SPACE methods. Moreover if we are
interested in handling events we specialize on CLIM:HANDLE-EVENT and... that is
all. No neat concepts like redisplay, output recording, presentations, clim
streams etc. They are very "neat" but require some extra effort to understand
them what may be hard at the beginning.

Our simplistic-tree-gadget may have two states: expanded and collapsed. We could
have tracked each individual tree node but it is not necessary for this
illustratory example. Later we'll get more fancy and use output-records.

|#

(defclass simplistic-tree-gadget (abstract-tree-gadget climi::always-repaint-background-mixin)
  ((expanded :initform nil :accessor expanded)
   (max-x :initform 250)
   (max-y :initform 60))
  (:default-initargs :text-style (clim:make-text-style :fixed nil :normal)))

(defmethod clim:compose-space ((pane simplistic-tree-gadget) &key width height)
  (declare (ignorable width height))
  (clim:make-space-requirement :min-width (slot-value pane 'max-x)
                               :width (slot-value pane 'max-x)
                               :max-width (slot-value pane 'max-x)
                               :min-height (slot-value pane 'max-y)
                               :height (slot-value pane 'max-y)
                               :max-height (slot-value pane 'max-y)))

(defmethod clim:handle-repaint ((pane simplistic-tree-gadget) region)
  (with-slots (max-x max-y) pane
    (setf max-x 0
          max-y 0))
  (let* ((text-style (clim:pane-text-style pane))
         (text-height (+ (clim:text-style-descent text-style pane)
                         (clim:text-style-ascent text-style pane)))
         (text-width (clim:text-style-width text-style pane))
         (current-y 0))
    (labels ((print-node (branch &optional (offset 0))
               (dolist (c (list-children branch))
                 (let ((node-text (cond ((null (list-children c))
                                         (format nil "  ~A" (name c)))
                                        ((expanded pane)
                                         (format nil "- ~A" (name c)))
                                        (t
                                         (format nil "+ ~A" (name c))))))
                   (clim:draw-text* pane node-text 0 current-y :align-y :top)
                   (incf current-y text-height)
                   (with-slots (max-x max-y) pane
                     (setf max-x (max 250 max-x (+ offset
                                                   (clim:text-size pane node-text)))
                           max-y (max 60 max-y current-y)))
                   (when (expanded pane)
                     (clim:with-translation (pane (* 4 text-width) 0)
                       (print-node c (+ offset (* 4 text-width)))))))))
      (print-node (slot-value pane 'root))))
  (clim:change-space-requirements pane :resize-frame t))

(defmethod clim:handle-event ((pane simplistic-tree-gadget)
                              (event clim:pointer-button-press-event))
  (setf (expanded pane) (not (expanded pane)))
  (clim:handle-repaint pane clim:+everywhere+))



#|

Simplistic gadget version 2
===========================

|#
(defclass simplistic-tree-gadget-v2 (abstract-tree-gadget climi::always-repaint-background-mixin)
  ((expanded :initform nil :accessor expanded)
   (buttons :initform (make-instance 'clim:standard-sequence-output-record)
            :accessor buttons)
   (max-x :initform 250)
   (max-y :initform 60))
  (:default-initargs :text-style (clim:make-text-style :fixed nil :normal)))

(defmethod expandedp ((pane simplistic-tree-gadget-v2) node)
  (member node (expanded pane)))

(defclass simplistic-output-record (climi::basic-output-record)
  ((node :initarg :node :reader node)))

(defmethod initialize-instance :after ((record simplistic-output-record)
                                       &key x1 y1 x2 y2)
  (setf (clim:rectangle-edges* record)
        (values x1 y1 x2 y2)))

(defmethod clim:compose-space ((pane simplistic-tree-gadget-v2) &key width height)
  (declare (ignorable width height))
  (clim:make-space-requirement :min-width (slot-value pane 'max-x)
                               :width (slot-value pane 'max-x)
                               :max-width (slot-value pane 'max-x)
                               :min-height (slot-value pane 'max-y)
                               :height (slot-value pane 'max-y)
                               :max-height (slot-value pane 'max-y)))

(defmethod clim:handle-repaint ((pane simplistic-tree-gadget-v2) region)
  (let* ((text-style (clim:pane-text-style pane))
         (text-height (+ (clim:text-style-descent text-style pane)
                         (clim:text-style-ascent text-style pane)))
         (text-width (clim:text-style-width text-style pane))
         (current-y 0))
    (clim:clear-output-record (buttons pane))
    (labels ((print-record (node)
               (clim:draw-rectangle* pane
                                     0
                                     current-y
                                     (* 3 text-width)
                                     (+ current-y text-height)
                                     :filled nil)
               (let ((x 0)
                     (y current-y)
                     (tr (climi::medium-transformation pane)))
                 (climi::with-transformed-position (tr x y)
                   (clim:add-output-record
                    (make-instance 'simplistic-output-record
                                   :node node
                                   :x1 x
                                   :y1 y
                                   :x2 (+ x (* 3 text-width))
                                   :y2 (+ y text-height))
                    (buttons pane)))))
             (print-node (branch)
               (dolist (c (list-children branch))
                 (let ((node-text (cond ((null (list-children c))
                                         (format nil "   ~A" (name c)))
                                        ((expandedp pane c)
                                         (format nil " - ~A" (name c)))
                                        (t
                                         (format nil " + ~A" (name c))))))
                   (print-record c)
                   (clim:draw-text* pane node-text 0 current-y :align-y :top)
                   (incf current-y text-height)
                   (when (expandedp pane c)
                     (clim:with-translation (pane (* 4 text-width) 0)
                       (print-node c)))))))
      (print-node (slot-value pane 'root)))
    (setf (slot-value pane 'max-y) current-y))
  (clim:change-space-requirements pane))

(defmethod clim:handle-event ((pane simplistic-tree-gadget-v2)
                              (event clim:pointer-button-press-event))
  (clim:map-over-output-records-containing-position
   #'(lambda (record)
       (let ((node (node record)))
         (if (member node (expanded pane))
             (setf (expanded pane) (delete node (expanded pane)))
             (push node (expanded pane)))
         (clim:handle-repaint pane clim:+everywhere+)
         (return-from clim:handle-event nil)))
   (buttons pane)
   (clim:pointer-event-x event)
   (clim:pointer-event-y event)))

#|

Simplistic gadget version 3
===========================

|#

(defclass simplistic-tree-gadget-v3 (abstract-tree-gadget) ())

#|

Material gadget
===============

https://dhtmlx.com/blog/suite-5-0-material-design-new-treeview-control/
https://vmware.github.io/clarity/documentation/v0.11/tree-view
|#


(defclass material-tree-gadget (abstract-tree-gadget) ())
