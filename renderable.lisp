(in-package #:lark)

(hasty:def-component mesh-renderable (:reactive transform)
    ((mesh (error "mesh must be supplied on construction of mesh-renderable")
	   :type yaksha:mesh))
  ;; proper rendering soon :)
  (with-transform (model->world) entity
    (let-model-space ((:to *world-space* model->world))
      (map-g #'first-render (mesh-stream mesh)))))

;; so the main loop can (hasty:run-pass *render-pass*)
(setf *render-pass* (hasty:get-system 'mesh-renderable))


;;----------------------------------------------------------------------

(defun-g first-vert ((v vertex))
  (in *clip-space*
    (values (in *model-space* (p! (pos v) 1.0))
	    (v! 1 0 0 0))))

(defun-g first-frag ((color :vec4))
  color)

(defpipeline first-render ()
    (g-> #'first-vert #'first-frag))
