(in-package #:lark)

(defun path-relative-to-lark (filepath)
  (asdf:system-relative-pathname :lark filepath))

(defvar *lark-dir*
  (path-relative-to-lark "./"))

(defvar *backup-tex* nil)
(defvar *backup-tex-sampler* nil)

(defun init-misc-data ()
  (setf *backup-tex* (cepl.devil:load-image-to-texture
		      (path-relative-to-lark "./suit/body_dif.png")))
  (setf *backup-tex-sampler* (sample *backup-tex*)))

(push #'init-misc-data *on-engine-init*)
