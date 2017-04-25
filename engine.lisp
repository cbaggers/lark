(in-package :lark)
(in-readtable fn:fn-reader)

(defun init-media ()
  (setf *camera* (cepl.camera:make-camera))
  (setf (viewport-resolution (viewport *camera*)) (v! 1024 768))
  (setf *catwalk*
        (sample
         (load-hdr-2d
          (path "media/pisa/pisa.hdr" t))))
  (setf *convolved-env*
        (sample
         (load-hdr-2d
          (path "media/pisa/pisa_diffuse.hdr" t))))
  (setf *game-state*
        (make-game-state
         :things (list (make-sphere-thing
                        (path "media/iron-rusted4/iron-rusted4-basecolor.png" t)
                        (path "media/iron-rusted4/iron-rusted4-normal.png" t)
                        (path "media/iron-rusted4/iron-rusted4-metalness.png" t)
                        (path "media/iron-rusted4/iron-rusted4-roughness.png" t)
                        :pos (v! 0 30 -120))
                       (make-sphere-thing
                        (path "media/bamboo-wood-semigloss-1/bamboo-wood-semigloss-albedo.png" t)
                        (path "media/bamboo-wood-semigloss-1/bamboo-wood-semigloss-normal.png" t)
                        (path "media/bamboo-wood-semigloss-1/bamboo-wood-semigloss-metal.png" t)
                        (path "media/bamboo-wood-semigloss-1/bamboo-wood-semigloss-roughness.png" t)
                        :pos (v! 50 -30 -120))
                       (make-sphere-thing
                        (path "media/scuffed-plastic-1/scuffed-plastic-alb.png" t)
                        (path "media/scuffed-plastic-1/scuffed-plastic-normal.png" t)
                        (path "media/scuffed-plastic-1/scuffed-plastic-metal.png" t)
                        (path "media/scuffed-plastic-1/scuffed-plastic-rough.png" t)
                        :pos (v! -50 -30 -120))))))

(defun start-engine ()
  (unless *started*
    (unless cepl.context:*gl-context*
      (cepl::init 640 480 "Lark" t))
    (setf *on-engine-init*
          (map nil #'funcall *on-engine-init*))
    (init-media)
    (skitter:listen-to (lambda (x y z)
                         (declare (ignore z))
                         (window-size-callback x y))
                       (skitter:window 0) :size)
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
                (gl:clear :color-buffer-bit :depth-buffer-bit)
                ;;(pile:with-tweak)
				(when (funcall main-loop-stepper)
				  (swank.live::continuable (step-game)))
                  ;; run render pass

				(swank.live::continuable (render *camera* *game-state*))
                (swap))
          (setf *running* nil)
          (print "-shutting down-")))))

(defun stop () (setf *running* nil))
