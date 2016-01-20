(in-package :lark)

(defvar *world-up* (v! 0 1 0))
(defvar *screen* nil)
(defvar *current-camera* nil)

(hasty:def-component eye (transform)
    ((space (space! (m4:identity))))
  nil
  )

(initialize-eye-system)

(defun make-camera ()
  (let ((c
	 (hasty:register-entity
	  (add-transform
	   (add-eye
	    (hasty:entity!))))))
    (unless *current-camera*
      (setf *current-camera* c))
    c))
