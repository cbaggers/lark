;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:jungl
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:jungl.space)
  (:shadow :space :texture :make-texture)
  (:export :load-model :vertex :mesh :make-mesh :model :make-model
	   :texture-jungl-texture :texture-type :y-texture
	   :vertex-position :vertex-normal :vertex-tex-coords
	   :mesh-stream :mesh-textures
	   :model-filepath :model-meshes :texture))

(defpackage #:lark
  (:shadow :space)
  (:import-from :cl-game-math.projection
		:perspective)
  (:use #:cl #:temporal-functions #:jungl #:jungl.space
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:yaksha))
