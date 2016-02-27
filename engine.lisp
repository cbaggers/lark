(in-package :lark)

(defvar *started* nil)

(defgeneric on-engine-start ())
(defmethod on-engine-start ())

(defmacro deflvar (name value)
  (if *started*
      `(defparameter ,name ,value)
      `(progn
	 (defvar ,name)
	 (defmethod on-engine-start :after ()
		    (setf ,name ,value)))))

(defun start-engine ()
  (unless *started*
    (setf *started* t)
    (unless jungl:*gl-context*
      (cepl:repl))
    (on-engine-start)
    (unless *current-camera*
      (setf *current-camera* (make-camera)))))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

(defparameter *step-func-name* '%%step)
(defparameter *running-var-name* '%%running)

;; {TODO} put this in main loop eventually
(defvar *render-pass* nil)

(defmacro defgame (name (&key startup-function) &body body)
  (let ((run-symb (cepl-utils:symb :run- name))
        (stop-symb (cepl-utils:symb :stop- name))
	(running-var (cepl-utils:symb :% name :-running)))
    `(progn
       (defvar ,running-var nil)

       (defun ,*step-func-name* ()
	 ,@body)

       (defun ,stop-symb () (setf ,running-var nil))

       (defun ,(cepl-utils:symb :is- name :-running?) ()
	 ,running-var)

       (defun ,run-symb (&optional for-frames)
	 (assert (or (null for-frames) (numberp for-frames)))
	 (unless *started*
	   (start-engine)
	   (unless *started*
	     (error "Lark: Cannot run ~s as engine could not be started."
		    ',name)))
	 (if ,running-var
	     (print "already running")
	     (let ((main-loop-stepper (temporal-functions:make-stepper
				       (seconds (/ 1.0 60.0))))
		   (swank-stepper (temporal-functions:make-stepper
				   (seconds (/ 1.0 10.0)))))
	       (format t "-starting-")
	       ,(when startup-function `(funcall ,startup-function))
	       (setf ,running-var t)
	       (unwind-protect
		    (loop :while (and ,running-var
				      (not (cepl:shutting-down-p))
				      (if for-frames
					  (> for-frames 0)
					  t))
		       :do
		       (when for-frames (decf for-frames))
		       ;; update swank
		       (when (funcall swank-stepper)
			 (swank.live::continuable (swank.live:update-swank)))
		       ;; update event system
		       (swank.live::continuable (cepl:step-host))
		       ;; update temporal pool
		       (swank.live::continuable (ttm:update))
		       ;; run step function
		       (when (funcall main-loop-stepper)
			 (swank.live::continuable (funcall #',*step-func-name*)))
		       ;; run all entity component system code (except rendering)
		       (swank.live::continuable (hasty:step-hasty))
		       ;; run render pass
		       (gl:clear :color-buffer-bit :depth-buffer-bit)
		       (swank.live::continuable (hasty:run-pass *render-pass*))
		       (swap))
		 (setf ,running-var nil)
		 (print "-shutting down-"))))
	 ',name))))
