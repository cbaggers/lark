(in-package #:lark)

(defvar *world-up* (v! 0 1 0))

(hasty:def-component eye (transform)
    ((ccam (cepl.camera:make-camera)))
  ;;
  (with-transform (position rotation) entity
    (setf (cepl.camera:camera-pos ccam) (v! (v:x position)
					    (v:y position)
					    (v:z position)
					    0)
	  (cepl.camera:camera-rot ccam) rotation)))

(defun make-camera ()
  (hasty:register-entity
   (add-eye
    (add-transform (hasty:entity!)
		   :position (v! 0 0 0)))))

(defmethod viewport ((c hasty:entity))
  (viewport (eye-ccam c)))

(defmethod (setf viewport) (value (c hasty:entity))
  (setf (viewport (eye-ccam c))
	value))

;;----------------------------------------------------------------------

(hasty:def-component god-mode (eye) ()
  (let ((mouse-pos (mouse-movement)))
    (setf (rot entity)
	  (q:* (rot entity)
	       (q:from-fixed-angles
	  	;;0
		(/ (y mouse-pos) 100.0)
	  	(/ (x mouse-pos) -100.0)
	  	0))))
  (let ((w (skitter:key-down-p key.w))
	(s (skitter:key-down-p key.s))
	(dir (q:to-look-at (rot entity))))
    (when w
      (setf (pos entity) (v3:+ (pos entity) (v3:*s dir 0.1))))
    (when s
      (setf (pos entity) (v3:- (pos entity) (v3:*s dir 0.1))))))

(defun reset-cam (cam)
  (setf (rot cam) (q:identity)
	(pos cam) (v! 0 0 0)))


(defmacro using-camera (camera &body body)
  `(with-eye (ccam) ,camera
     (cepl.camera:using-camera ccam
       ,@body)))
