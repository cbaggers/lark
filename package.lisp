;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:jungl
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:jungl.space)
  (:shadow :space)
  (:export :load-model :vertex :mesh :make-mesh :model :make-model))

(defpackage #:lark
  (:shadow :space)
  (:use #:cl #:temporal-functions #:jungl #:jungl.space
	#:cl-game-math.base-vectors #:cl-game-math.base-matrices
	#:yaksha))
