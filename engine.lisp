(in-package :lark)

(defvar *started* nil)
(defvar *on-engine-init* nil)

(defmacro deflvar (name value)
  (if *started*
      `(defparameter ,name ,value)
      `(progn
	 (defvar ,name)
	 (defmethod on-engine-start :after ()
		    (setf ,name ,value)))))

(defun start-engine ()
  (unless *started*
    (unless jungl:*gl-context*
      (cepl::init 640 480 "Lark" t))
    (map nil #'funcall *on-engine-init*)
    (setf *on-engine-init* nil)
    (unless *current-camera*
      (setf *current-camera* (make-camera)))
    (setf *started* t)))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

(defparameter *step-func-name* '%%step)
(defparameter *running-var-name* '%%running)

;; {TODO} put this in main loop eventually
(defvar *render-pass* nil)

(defun update-internals ()
  (swap-mouse-move))


(defvar *fps* 0)

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
				   (seconds (/ 1.0 10.0))))
		   (fps-stepper (temporal-functions:make-stepper
				 (seconds 1)))
		   (fps-frame-count 0))
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
		       ;; update fps
		       (incf fps-frame-count)
		       (when (funcall fps-stepper)
			 (setf *fps* fps-frame-count
			       fps-frame-count 0))
		       ;; update event system
		       (swank.live::continuable (cepl:step-host))
		       ;; update lark internal
		       (update-internals)
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
		       (render-sky)
		       (swap))
		 (setf ,running-var nil)
		 (print "-shutting down-"))))
	 ',name))))
