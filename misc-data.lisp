(in-package #:assurance)

(defun path (filepath)
  (asdf:system-relative-pathname :assurance filepath))

(defvar *backup-tex* nil)
(defvar *backup-tex-sampler* nil)

(defparameter *quad* nil)
(defparameter *quad-stream* nil)

(defun init-misc-data ()
  (setf *backup-tex* (cepl.sdl2-image:load-image-to-texture
		      (path "./suit/body_dif.png")))
  (setf *backup-tex-sampler* (sample *backup-tex*))
  (setf *quad*
	(make-gpu-array
	 (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
	       (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
	       (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
	       (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
	       (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
	       (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
	 :element-type 'g-pt
	 :dimensions 6))
  (setf *quad-stream*
	(make-buffer-stream *quad* :retain-arrays t)))

(push #'init-misc-data *on-engine-init*)
