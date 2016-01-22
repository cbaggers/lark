;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:jungl
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:jungl.space)
  (:shadow :space)
  (:export :load-model :vertex :mesh :make-mesh :model :make-model
	   :texture-jungl-texture :texture-type
	   :vertex-position :vertex-normal :vertex-tex-coords
	   :mesh-stream :mesh-textures
	   :model-filepath :model-meshes))

(defpackage #:lark
  (:shadow :space)
  (:import-from :cl-game-math.projection
		:perspective)
  (:use #:cl #:temporal-functions #:jungl #:jungl.space
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:yaksha))
