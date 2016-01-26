(in-package #:lark)

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g first-vert ((vert vertex) &uniform (mspace space-g))
  (in *clip-space*
    (values (in mspace (p! (pos vert) 1.0))
	    (v! (uv vert) (uv vert)))))

;; (defun-g second-vert ((vert vertex) &uniform (bah :mat4))
;;   (values (* bah (v! (pos vert) 1.0))
;; 	  (v! (uv vert) (uv vert))))

(defun-g first-frag ((color :vec4))
  color)

(defpipeline first-render ()
    (g-> #'first-vert #'first-frag))

;; (defpipeline second-render ()
;;     (g-> #'second-vert #'first-frag))

;;----------------------------------------------------------------------
;; system
(progn
  (hasty:def-component renderable (:reactive transform)
      ((mesh (error "model must be supplied on construction of mesh-renderable")
	     :type yaksha:mesh)
       (space (space! *world-space*) :type space))
    ;;
    ;; populate space from transform
    (with-transform (position rotation) entity
      (setf (get-transform space *world-space*)
	    (m4:m* (m4:translation position)
		   (m4:rotation-from-euler rotation))))
    ;; draw some stuff
    (with-eye (ccam) *current-camera*
      (cepl.camera:using-camera ccam
	(map-g #'first-render (mesh-stream mesh) :mspace space)))

    ;; ;;
    ;; ;; populate space from transform
    ;; (with-transform (position rotation) entity
    ;;   (setf (get-transform space *world-space*)
    ;; 	    (m4:m* (m4:translation (v:- position (v! 7 0 0 0)))
    ;; 		   (m4:rotation-from-euler rotation))))
    ;; ;; draw some stuff
    ;; (with-eye (ccam) *current-camera*
    ;;   (cepl.camera:using-camera ccam
    ;; 	(map-g #'second-render (mesh-stream mesh)
    ;; 	       :bah (get-transform space *clip-space*))))
    )

  ;; so the main loop can (hasty:run-pass *render-pass*)
  (setf *render-pass* (hasty:get-system 'renderable)))

;;----------------------------------------------------------------------
