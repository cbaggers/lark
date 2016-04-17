;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:cepl #:rtg-math)
  (:shadow :texture :make-texture)
  (:export :vertex
	   :mesh :make-mesh
	   :model :load-model :make-model-from-mesh
	   :texture-cepl-texture :texture-type :y-texture
	   :vertex-position :vertex-normal :vertex-tex-coords
	   :mesh-stream :mesh-textures :mesh-samplers
	   :model-filepath :model-meshes :texture
	   :uv :normal))

(defpackage #:lark
  (:import-from :rtg-math.projection :perspective)
  (:import-from :yaksha :uv :normal :normal)
  (:use #:cl #:temporal-functions #:cepl
	#:varjo-lang #:rtg-math #:skitter.sdl2.keys
	#:skitter.sdl2.mouse-buttons
	#:structy-defclass))

(in-package #:lark)

(defun %run-session ()
  #+darwin
  (let ((extra-package-dirs '("/opt/local/lib/" "/usr/local/")))
    (mapcar
     (lambda (raw-path)
       (let ((port-dir (cl-fad:directory-exists-p raw-path)))
         (when (and port-dir
                    (not (member port-dir cffi:*foreign-library-directories*)))
           (push port-dir cffi:*foreign-library-directories*))))
     extra-package-dirs))
  (let (#+linux
        (style swank::*communication-style*)
        #-linux
        (style nil))
    (cepl.host:set-primary-thread-and-run
     (lambda () (swank:create-server :style style :dont-close t)))))
