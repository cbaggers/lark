(in-package :lark)

(hasty:def-component eye () ()
  nil)

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

(defvar *screen* nil)
(defvar *current-camera* nil)
