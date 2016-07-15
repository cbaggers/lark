;;;; assurance.asd

(asdf:defsystem #:assurance
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.sdl2 #:cepl.camera #:cepl.sdl2-image #:cepl.skitter.sdl2
			   #:classimp #:fn #:named-readtables #:cl-fad
			   #:temporal-functions #:dendrite #:disposable
			   #:structy-defclass #:swank.live
			   #:filmic-tone-mapping-operators)
  :components ((:file "package")

	       ;; mesh and models
               (:file "yaksha/mesh")
	       (:file "yaksha/model")

	       ;; vars
	       (:file "vars")

	       ;; useful data
	       (:file "misc-data")

	       ;; control
	       (:file "mouse")

	       ;;
	       (:file "camera")
	       (:file "thing")
	       (:file "primitives")

	       ;; pbr
	       (:file "importance-sample-cube-diffuse")
	       (:file "importance-sample-cube-specular")
	       (:file "pbr")
	       (:file "dfg-texture")

	       ;; sky
	       (:file "sky/sky")

	       ;; hmm
	       (:file "engine")))
