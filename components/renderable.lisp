(in-package #:lark)

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g first-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (in *clip-space*
    (values (in model-space (sv! (pos vert) 1.0))
	    (pos vert)
	    (yaksha:normal vert)
	    (yaksha:uv vert))))

(defun-g first-frag ((norm :vec3) (pos :vec3) (uv :vec2) &uniform (tex :sampler-2d)
		     (camera-space vec-space) (model-space vec-space)
		     (light-pos :vec3) (light-intensity :float))
  (let ((sun-cos-angle-of-incidence
	  (varjo-lang:clamp
	   (in camera-space
	     (varjo-lang:dot (v:normalize (in model-space (sv! norm 0)))
			     (in *world-space* (sv! *dir-to-sun*))))
	   0 1))
	(diffuse-color (varjo-lang:texture tex uv))
	(light-cos-angle-of-incidence
	 (varjo-lang:clamp
	  (in model-space
	    (varjo-lang:dot (v:normalize (sv! norm 0))
			    (sv! (v:normalize
				 (- (in *world-space* (sv! light-pos 0))
				    (v! pos 0))))))
	  0 1)))
    (+ (* diffuse-color *ambient-intensity*)
       (* light-intensity diffuse-color light-cos-angle-of-incidence)
       ;;(* *sun-intensity* diffuse-color sun-cos-angle-of-incidence)
       )))

(def-g-> first-render ()
  #'first-vert #'first-frag)

;;----------------------------------------------------------------------
;; system
(progn
  (hasty:def-component renderable (:reactive transform)
      ((model (error "model must be supplied on construction of mesh-renderable")
	      :type yaksha:model)
       (space (make-space *world-space* (m4:identity)) :type vec-space))

    ;; populate space from transform
    (with-transform (position rotation) entity
      (setf (get-transform space *world-space*)
	    (m4:* (m4:translation position)
		  (q:to-mat4 rotation))))


    (let ((light-pos
	   ;; (v! 0
	   ;;     (+ 25 (* 30 (sin (/ (get-internal-real-time) 800))))
	   ;;     -0)
	   (v! (* (cos (/ (get-internal-real-time) 600)) 50)
	       100
	       (+ -20 (* (sin (/ (get-internal-real-time) 600)) 50)))
	    ))

      ;; draw some stuff
      (with-viewport (current-viewport)
	(using-camera *current-camera*
	  (loop :for mesh :in (yaksha:model-meshes model) :do
	     (map-g #'first-render (yaksha:mesh-stream mesh)
		    :model-space space
		    :camera-space (cepl.camera.base::base-camera-space ccam)
		    :tex (let ((y-tex (first (yaksha:mesh-samplers mesh))))
			   (if y-tex
			       (yaksha::texture-cepl-texture y-tex)
			       *backup-tex-sampler*))
		    :light-pos light-pos
		    :light-intensity 0.5))))))

  ;; so the main loop can (hasty:run-pass *render-pass*)
  (setf *render-pass* (hasty:get-system 'renderable)))

;;----------------------------------------------------------------------
