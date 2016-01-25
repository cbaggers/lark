;;;; lark.asd

(asdf:defsystem #:lark
  :description "An experient to find the working state of cepl"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (#:cepl-default #:classimp #:fn #:named-readtables
			      #:cepl.camera
			      #:cepl-image-helper #:cl-fad
			      #:temporal-functions #:hasty
			      #:dendrite #:disposable)
  :components ((:file "package")

	       ;; mesh and models
               (:file "yaksha/mesh")
	       (:file "yaksha/model")

	       ;; lark core api
	       (:file "pools")
	       (:file "engine")

	       ;; components
	       (:file "transform")
	       (:file "camera")
	       (:file "renderable")

	       ;; api
	       (:file "api")))
