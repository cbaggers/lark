(in-package #:lark)

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g first-vert ((vert vertex) &uniform (mspace space-g))
  (in *clip-space*
    (values (in mspace (p! (pos vert) 1.0))
	    (uv vert))))

(defun-g first-frag ((uv :vec2) &uniform (tex :sampler-2d))
  (varjo-lang:texture tex uv))

(defpipeline first-render ()
    (g-> #'first-vert #'first-frag))

;;----------------------------------------------------------------------
;; system
(progn
  (hasty:def-component renderable (:reactive transform)
      ((model (error "model must be supplied on construction of mesh-renderable")
	      :type yaksha:model)
       (space (space! *world-space*) :type space))

    ;; populate space from transform
    (with-transform (position rotation) entity
      (setf (get-transform space *world-space*)
	    (m4:m* (m4:translation position)
		   (q:to-matrix4 rotation))))

    ;; draw some stuff
    (with-eye (ccam) *current-camera*
      (cepl.camera:using-camera ccam
	(loop :for mesh :in (model-meshes model) :do
	   (map-g #'first-render (mesh-stream mesh) :mspace space
		  :tex (or (first (mesh-textures mesh))
			   *backup-tex*))))))

  ;; so the main loop can (hasty:run-pass *render-pass*)
  (setf *render-pass* (hasty:get-system 'renderable)))

;;----------------------------------------------------------------------
