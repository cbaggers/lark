(in-package #:lark)

(hasty:def-component mesh-renderable (transform)
    ((mesh (error "mesh must be supplied on construction of mesh-renderable")
	   :type yaksha:mesh))
  ;; proper rendering soon :)
  (format t "~%render mesh ~s" mesh-renderable-mesh))

(initialize-mesh-renderable-system)
