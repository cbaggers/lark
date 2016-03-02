(in-package #:lark)

(defun path-relative-to-lark (filepath)
  (asdf:system-relative-pathname :lark filepath))

(defvar *lark-dir*
  (path-relative-to-lark "./"))

(defvar *backup-tex* nil)

(defun init-misc-data ()
  (setf *backup-tex* (cepl.devil:load-image-to-texture
		      (path-relative-to-lark "./suit/body_dif.png"))))

(push #'init-misc-data *on-engine-init*)
