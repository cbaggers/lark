;;;; assurance.asd

(asdf:defsystem #:assurance
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.sdl2 #:cepl.camera #:cepl.devil #:cepl.skitter.sdl2
			   #:classimp #:fn #:named-readtables #:cl-fad
			   #:temporal-functions #:dendrite #:disposable
			   #:structy-defclass #:swank.live)
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
	       (:file "lights")
	       (:file "camera")
	       (:file "thing")
	       (:file "primitives")

	       ;; sky
	       (:file "sky/sky")

	       ;; hmm
	       (:file "engine")))
