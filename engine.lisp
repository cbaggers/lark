(in-package :lark)
(in-readtable fn:fn-reader)

(defvar *started* nil)
(defvar *game-state* nil)

(defun start-engine ()
  (unless *started*
    (unless cepl.context:*gl-context*
      (cepl::init 640 480 "Lark" t))
    (setf *on-engine-init*
	  (map nil #'funcall *on-engine-init*))
    (setf *camera* (cepl.camera:make-camera))
    (setf (viewport-resolution (viewport *camera*)) (v! 1024 768))
    (setf *game-state*
	  (make-game-state
	   :things (list (load-thing
			  "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_LP.FBX"
			  "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_A.png"
			  "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_N.png"
			  "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_M_R.png"))))
    (skitter:listen-to λ(window-size-callback _ _1) (skitter:window 0) :size)
    (setf *started* t)))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

;;----------------------------------------------------------------------
;; Run/Stop

(defvar *fps* 0)
(defvar *running* nil)

(defun is-running-p ()
  *running*)

(defun run (&optional for-frames)
  (assert (or (null for-frames) (numberp for-frames)))
  (unless *started*
    (start-engine)
    (unless *started*
      (error "Lark: Cannot run as engine could not be started.")))
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
		(swank.live::continuable (render *camera* *game-state*)))
	  (setf *running* nil)
	  (print "-shutting down-")))))

(defun stop () (setf *running* nil))
