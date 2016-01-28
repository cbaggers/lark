(in-package #:lark)

(defvar *world-up* (v! 0 1 0))
(defvar *screen* nil)
(defvar *current-camera* nil)

(hasty:def-component eye (transform)
    ((ccam (cepl.camera:make-camera)))
  ;;
  (with-transform (position rotation) entity
    (setf (cepl.camera:camera-pos ccam) position
	  (cepl.camera:camera-rot ccam) rotation)))

(defun make-camera ()
  (hasty:register-entity
   (add-eye
    (add-transform (hasty:entity!)
		   :position (v! 0 0 0 0)))))
