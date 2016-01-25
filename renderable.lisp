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
    ((mesh (error "model must be supplied on construction of mesh-renderable")
	    :type yaksha:mesh)
     (space (space! *world-space*) :type space))
  ;;
  ;; populate space from transform
  (with-transform (position rotation) entity
    (setf (get-transform space *world-space*)
	  (m4:m* (m4:translation position)
		 (q:to-matrix4 (q:normalize rotation)))))
  ;; draw some stuff
  (with-eye (ccam) *current-camera*
    (cepl.camera:using-camera ccam
      (map-g #'first-render (mesh-stream mesh) :model-space space))))

;; so the main loop can (hasty:run-pass *render-pass*)
(setf *render-pass* (hasty:get-system 'renderable))
