;;;; lark.asd

(asdf:defsystem #:lark
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (#:cepl.sdl2 #:cepl.camera #:cepl.devil #:cepl.skitter.sdl2
			   #:classimp #:fn #:named-readtables
			   #:cl-fad #:temporal-functions #:hasty
			   #:dendrite #:disposable
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

	       ;; components
	       (:file "components/transform")
	       (:file "components/camera")
	       (:file "components/renderable")

	       ;; api
	       (:file "api")))
