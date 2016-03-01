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
		   :position (v! 0 0 0 0)))))

(defmethod viewport ((c hasty:entity))
  (cepl.camera:viewport (eye-ccam c)))

(defmethod (setf viewport) (value (c hasty:entity))
  (setf (cepl.camera:viewport (eye-ccam c))
	value))

;;----------------------------------------------------------------------

(hasty:def-component god-mode (eye) ()
  (let ((mouse-pos (skitter:mouse-pos (skitter:mouse 0))))
    (setf (rot entity) (q:from-fixed-angles
			(/ (y (skitter:xy-pos-vec mouse-pos)) 300.0)
			(/ (x (skitter:xy-pos-vec mouse-pos)) 300.0)
			0)))
  (let ((w (skitter:key-down-p key.w))
	(s (skitter:key-down-p key.s))
	(dir (q:to-look-at (rot entity))))
    (when w
      (setf (pos entity) (v3:+ (pos entity) (v3:*s dir 0.01))))
    (when s
      (setf (pos entity) (v3:- (pos entity) (v3:*s dir 0.01))))))
