(in-package #:lark)

(hasty:def-component mesh-renderable (:reactive transform)
    ((mesh (error "mesh must be supplied on construction of mesh-renderable")
	   :type yaksha:mesh))
  ;; proper rendering soon :)
  (cepl:cls))

(initialize-mesh-renderable-system)

(setf *render-pass* (hasty:get-system 'mesh-renderable))


;;----------------------------------------------------------------------

(defun-g first-vert ((v vertex))
  (in *clip-space*
    (values (in *model-space* (p! (pos v) 1.0))
	    (v! 1 0 0 0))))

(defun-g first-frag ((color :vec4))
  color)

(defpipeline first-render ()
    (g-> #'va6 #'fa5))
