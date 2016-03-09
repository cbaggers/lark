;;;; lark.asd

(asdf:defsystem #:lark
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.sdl2 #:cepl.camera #:cepl.devil #:cepl.skitter.sdl2
			   #:classimp #:fn #:named-readtables
			   #:cl-fad #:temporal-functions #:hasty
			   #:dendrite #:disposable
			   #:structy-defclass
			   #:swank.live)
  :components ((:file "package")

	       ;; mesh and models
               (:file "yaksha/mesh")
	       (:file "yaksha/model")

	       ;; lark core api
	       (:file "state")
	       (:file "pools")
	       (:file "engine")
	       (:file "lights")

	       ;; useful data
	       (:file "misc-data")

	       ;; control
	       (:file "mouse")

	       ;; components
	       (:file "components/transform")
	       (:file "components/camera")
	       (:file "components/renderable")

	       ;; sky
	       (:file "sky/sky")

	       ;; api
	       (:file "api")))
