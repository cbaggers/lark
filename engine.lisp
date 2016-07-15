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
(defvar gun nil)

(defun startup ()
  (unless (things *game-state*)
    (let ((lptex-d (make-texture nil :dimensions '(128 128) :cubes t
				 :element-type :rgb16f :mipmap t))
	  (lptex-s (make-texture nil :dimensions '(128 128) :cubes t
				 :element-type :rgb16f :mipmap t)))
      (setf light-probe-sampler (sample lptex-d)
	    light-probe-fbo (make-fbo lptex-d :d))
      (setf light-probe-specular-sampler (sample lptex-s)
	    light-probe-specular-fbo (make-fbo lptex-s :d)))
    (setf gun (load-thing
	       "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_LP.FBX"
	       "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_A.png"
	       "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_N.png"
	       "/home/baggers/Code/lisp/lark/media/Cerberus_by_Andrew_Maximov/Cerberus_M_R.png"))
    (setf qoob
	  (sample
	   (load-hdr-cross-texture "/home/baggers/Downloads/galileo_cross.hdr")))
    (let ((dfg-tex (make-texture nil :dimensions '(128 128)
				 :element-type :rgb16f)))
      ;; apparently this could be r16g16f (rg16f?) ↑  ↑
      (setf dfg-sampler (sample dfg-tex))
      (setf dfg-fbo (make-fbo `(0 ,dfg-tex) '(:d :dimensions '(128 128)))))
    (push gun (things *game-state*))
    (setf (pos gun) (v! 0 0 -160))))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (reshape (skitter:size-2d-vec event)))

(defvar auto-rot t)

(defun step-game ()
  (swap-mouse-move)
  (when (skitter:key-down-p key.1)
    (setf auto-rot t))
  (when (skitter:key-down-p key.2)
    (setf auto-rot nil))
  (let ((time (/ (now) 10200))
	(thing gun))
    (if auto-rot
	(setf (rot thing) (q:from-mat3
			   (m3:rotation-from-euler
			    (v! (* 2 (cos time))
				(sin time)
				(sin time)))))
	(let ((v (v2:/s (mouse-pos) 100s0)))
	  (setf (rot thing) (q:from-mat3
			     (m3:rotation-from-euler
			      (v! (y v) 0s0 (x v)))))))))

(defun render ()
  (let ((camera *camera*))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (map-g-into dfg-fbo #'dfg-texture-pass *quad-stream*)
    (map-g-into light-probe-fbo #'diffuse-sample-hdr-cube *quad-stream*
		:value-multiplier 1s0 :cube qoob)
    (map-g-into light-probe-fbo #'specular-sample-hdr-cube *quad-stream*
		:value-multiplier 1s0 :cube qoob :roughness 0.1)
    (with-fbo-bound ((post-buff-fbo (get-post-buff)))
      (clear))
    (map nil λ(render-thing (update-thing _) camera)
    	 (things *game-state*))
    (render-sky camera)
    (post-proc camera)
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
