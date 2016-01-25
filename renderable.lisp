(in-package #:lark)

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g first-vert ((v vertex) &uniform (model-space space-g))
  (in *clip-space*
    (values (in model-space (p! (pos v) 1.0))
	    (v! 1 0 0 0))))

(defun-g first-frag ((color :vec4))
  color)

(defpipeline first-render ()
    (g-> #'first-vert #'first-frag))

;;----------------------------------------------------------------------
;; system

(hasty:def-component renderable (:reactive transform)
    ((model (error "model must be supplied on construction of mesh-renderable")
	    :type yaksha:model)
     (space (space! *world-space*) :type space))
  ;;
  ;; populate space from transform
  (with-transform (position rotation) entity
    (update
     :model->world (m4:m* (m4:translation position)
			  (q:to-matrix4 (q:normalize rotation)))))

  ;; draw some stuff
  (cepl.camera:using-camera *current-camera*
    (map-g #'first-render (mesh-stream mesh) :model-space space)))

;; so the main loop can (hasty:run-pass *render-pass*)
(setf *render-pass* (hasty:get-system 'renderable))
