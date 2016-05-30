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

(defpackage #:assurance
  (:import-from :rtg-math.projection :perspective)
  (:import-from :yaksha :uv :normal :normal)
  (:import-from #:cepl.camera
		:export :make-camera :in-space :fov
		:cam->clip :x->cam :using-camera :camera-pos :camera-rot)
  (:use #:cl #:temporal-functions #:cepl #:named-readtables
	#:varjo-lang #:rtg-math #:skitter.sdl2.keys
	#:skitter.sdl2.mouse-buttons
	#:structy-defclass)
)
;; :near :far :perspective
