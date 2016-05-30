(in-package #:assurance)

(defun path (filepath)
  (asdf:system-relative-pathname :assurance filepath))

(defvar *backup-tex* nil)
(defvar *backup-tex-sampler* nil)

(defun init-misc-data ()
  (setf *backup-tex* (cepl.devil:load-image-to-texture
		      (path "./suit/body_dif.png")))
  (setf *backup-tex-sampler* (sample *backup-tex*)))

(push #'init-misc-data *on-engine-init*)
