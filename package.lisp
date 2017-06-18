;;;; package.lisp

(defpackage #:yaksha
  (:use #:cl #:named-readtables #:cepl-utils #:cepl #:rtg-math
        #:cepl.spaces)
  (:shadow :texture :make-texture)
  (:export :vertex
           :mesh :make-mesh
           :model :load-model :make-model-from-mesh
           :y-texture-cepl-texture :y-texture-type :y-texture
           :vertex-position :vertex-normal :vertex-tex-coords
           :mesh-stream :mesh-textures :mesh-samplers
           :model-filepath :model-meshes :texture
           :uv :normal :tangent))

(defpackage #:lark
  (:import-from :rtg-math.projection :perspective)
  (:import-from :yaksha :uv :normal :normal)
  (:import-from #:cepl.camera
                :export :make-camera :in-space :fov
                :cam->clip :x->cam :using-camera :camera-pos :camera-rot
                :camera-viewport)
  (:use #:cl #:temporal-functions #:cepl #:named-readtables
        #:vari #:rtg-math :rtg-math.base-maths :cepl.skitter
        #:structy-defclass #:nineveh
        #:cepl.spaces))
;; :near :far :perspective
