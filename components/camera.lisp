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
  (cepl.camera:viewport (eye-ccam c)))

(defmethod (setf viewport) (value (c hasty:entity))
  (setf (cepl.camera:viewport (eye-ccam c))
	value))

;;----------------------------------------------------------------------

;; {TODO} try adding to something. then removing...it acted like it was
;;        still attached, until Ι recompiled the def-component below.
;;        odd.. odder it happened with add too..WAT

(hasty:def-component god-mode (eye) ()
  (let ((mouse-pos (mouse-movement)))
    (setf (rot entity)
	  (q:* (rot entity)
	       (q:from-fixed-angles
		(/ (y mouse-pos) 1000.0)
		(/ (x mouse-pos) -1000.0)
		0))))
  (let ((w (skitter:key-down-p key.w))
	(s (skitter:key-down-p key.s))
	(dir (q:to-look-at (rot entity))))
    (when w
      (setf (pos entity) (v3:+ (pos entity) (v3:*s dir 0.1))))
    (when s
      (setf (pos entity) (v3:- (pos entity) (v3:*s dir 0.1))))))
