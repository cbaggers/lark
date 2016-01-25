(in-package #:lark)

(defvar *world-up* (v! 0 1 0))
(defvar *screen* nil)
(defvar *current-camera* nil)

(hasty:def-component eye (transform)
    ((ccam (cepl.camera:make-camera)))
  (declare (optimize (speed 0) (debug 3)))
  ;;
  (with-transform (position rotation) entity
    (setf (camera-pos ccam) position
	  (camera-rot ccam) rotation)))

(defun make-camera ()
  (let ((c (hasty:register-entity
	    (add-eye
	     (add-transform (hasty:entity!)
			    :position (v! 0 0 10))))))
    (unless *current-camera*
      (setf *current-camera* c))
    c))
