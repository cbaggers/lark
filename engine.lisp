(in-package :assurance)
(in-readtable fn:fn-reader)

(defvar *started* nil)
(defvar *game-state* nil)

(deftclass (game-state (:conc-name nil))
  things)

(defun start-engine ()
  (unless *started*
    (unless cepl.context:*gl-context*
      (cepl::init 640 480 "Assurance" t))
    (setf *on-engine-init*
	  (map nil #'funcall *on-engine-init*))
    (setf *game-state* (make-game-state))
    (setf *camera* (cepl.camera:make-camera))
    (setf (viewport-resolution (viewport *camera*)) (v! 1024 768))
    (skitter:listen-to λ(window-size-callback _ _1) (skitter:window 0) :size)
    (setf *started* t)))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

(defun now ()
  (get-internal-real-time))

(defvar *render-pass* nil)

(defvar *fps* 0)

(defvar *running* nil)
(defvar gnaw nil)

(defun startup ()
  (unless (things *game-state*)
    (setf gnaw (load-thing
		"/home/baggers/Code/lisp/lark/media/GnawTheGoblin/Goblin_Gnaw.FBX"
		"/home/baggers/Code/lisp/lark/media/GnawTheGoblin/Goblin_Gnaw_Low_Diffuse.tga"
		"/home/baggers/Code/lisp/lark/media/GnawTheGoblin/Goblin_Gnaw_Low_Normal.tga"
		"/home/baggers/Code/lisp/lark/media/GnawTheGoblin/Goblin_Gnaw_Low_Metalic.tga"))
    (push gnaw (things *game-state*))
    (setf (pos (first (things *game-state*))) (v! 0 0 -120))))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (reshape (skitter:size-2d-vec event)))

(defun step-game ()
  (swap-mouse-move))

(defun render ()
  (let ((camera *camera*))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (render-sky camera)
    (map nil λ(render-thing (update-thing _) camera)
	 (things *game-state*))
    (swap)))

(defun stop () (setf *running* nil))

(defun is-running-p ()
  *running*)

(defun run (&optional for-frames)
  (assert (or (null for-frames) (numberp for-frames)))
  (unless *started*
    (start-engine)
    (unless *started*
      (error "Assurance: Cannot run as engine could not be started.")))
  (if *running*
      (print "already running")
      (let ((main-loop-stepper (temporal-functions:make-stepper
				(seconds (/ 1.0 60.0))))
	    (swank-stepper (temporal-functions:make-stepper
			    (seconds (/ 1.0 10.0))))
	    (fps-stepper (temporal-functions:make-stepper
			  (seconds 1)))
	    (fps-frame-count 0))
	(format t "-starting-")
	(startup)
	(setf *running* t)
	(unwind-protect
	     (loop :while (and *running*
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
		;; update temporal pool
		(swank.live::continuable (ttm:update))
		;; run step function
		(when (funcall main-loop-stepper)
		  (swank.live::continuable (step-game)))
		;; run render pass
		(swank.live::continuable (render)))
	  (setf *running* nil)
	  (print "-shutting down-")))))
