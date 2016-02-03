;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:jungl
	#:rtg-math #:jungl.space)
  (:shadow :space :texture :make-texture)
  (:export :vertex
	   :mesh :make-mesh
	   :model :load-model :make-model-from-mesh
	   :texture-jungl-texture :texture-type :y-texture
	   :vertex-position :vertex-normal :vertex-tex-coords
	   :mesh-stream :mesh-textures
	   :model-filepath :model-meshes :texture
	   :uv :normal))

(defpackage #:lark
  (:shadow :space)
  (:import-from :rtg-math.projection
		:perspective)
  (:import-from :temporal-functions :real-seconds)
  (:use #:cl #:temporal-functions #:jungl #:jungl.space
	#:varjo-lang #:rtg-math))
