(in-package #:lark)

(defvar *world-up* (v! 0 1 0))
(defvar *screen* nil)
(defvar *current-camera* nil)

(hasty:def-component eye (transform)
    ((space (space! (m4:identity)))
     (frame-size (jungl:viewport-resolution-v! (jungl:current-viewport))
		 :type (simple-array single-float (2)))
     (near 1.0 :type single-float)
     (far 1000.0 :type single-float)
     (fov 120.0 :type single-float))
  ;; now the pass. first we calculate our projection
  (let ((cam->clip (perspective (v:x frame-size) (v:y frame-size)
				near far fov)))
    ;; and set our relationship with clip space
    (update-non-hierarchical-relationship
     space *clip-space* cam->clip nil)
    ;; and set our relationship with world space
    (with-transform (model->world) entity
      (update-non-hierarchical-relationship
       space *world-space* model->world (m4:affine-inverse model->world)))))

(defun make-camera ()
  (let ((c
	 (hasty:register-entity
	  (add-transform
	   (add-eye
	    (hasty:entity!))))))
    (unless *current-camera*
      (setf *current-camera* c))
    c))
