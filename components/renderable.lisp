(in-package #:lark)

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g first-vert ((vert yaksha:vertex) &uniform (model-space space-g))
  (in *clip-space*
    (values (in model-space (p! (pos vert) 1.0))
	    (pos vert)
	    (yaksha:normal vert)
	    (yaksha:uv vert))))

(defun-g first-frag ((norm :vec3) (pos :vec3) (uv :vec2) &uniform (tex :sampler-2d)
		     (camera-space space-g) (model-space space-g)
		     (light-pos :vec3) (light-intensity :float))
  (let ((sun-cos-angle-of-incidence
	  (varjo-lang:clamp
	   (in camera-space
	     (varjo-lang:dot (v:normalize (in model-space (p! norm 0)))
			     (in *world-space* (p! *dir-to-sun*))))
	   0 1))
	(diffuse-color (varjo-lang:texture tex uv))
	(light-cos-a-of-i
	 (varjo-lang:clamp
	  (in model-space
	    (varjo-lang:dot (v:normalize (p! norm 0))
			    (p! (- (in *world-space* (p! light-pos 0))
				   (v! pos 0)))))
	  0 1)))
    (+ (* diffuse-color *ambient-intensity*)
       (* light-intensity diffuse-color light-cos-a-of-i)
       ;;(* *sun-intensity* diffuse-color sun-cos-angle-of-incidence)
       )))

(defpipeline first-render ()
    (g-> #'first-vert #'first-frag))

;; (defun-g test ((norm :vec3) (pos :vec3) (uv :vec2))
;;   (in *clip-space*
;;     (in *screen-space*
;;       (p! (v! 0 0 0 0)))))

;; (defpipeline woah ()
;;     (g-> #'first-vert #'test))

;;----------------------------------------------------------------------
;; system
(progn
  (hasty:def-component renderable (:reactive transform)
      ((model (error "model must be supplied on construction of mesh-renderable")
	      :type yaksha:model)
       (space (space! *world-space* (m4:identity)) :type space))

    ;; populate space from transform
    (with-transform (position rotation) entity
      (setf (get-transform space *world-space*)
	    (m4:m* (m4:translation position)
		   (q:to-matrix4 rotation))))


    (let ((light-pos (v! 0
			 (+ 25 (* 30 (sin (/ (get-internal-real-time) 800))))
			 -0)
	   ;; (v! (* (cos (/ (get-internal-real-time) 600)) 50)
	   ;;     (* 10 (sin (/ (get-internal-real-time) 100)))
	   ;;     (+ -20 (* (sin (/ (get-internal-real-time) 600)) 50)))
	    ))

      ;; draw some stuff
      (with-viewport (current-viewport)
	(with-eye (ccam) *current-camera*
	  (cepl.camera:using-camera ccam
	    (loop :for mesh :in (yaksha:model-meshes model) :do
	       (map-g #'first-render (yaksha:mesh-stream mesh)
		      :model-space space
		      :camera-space (cepl.camera.base::base-camera-space ccam)
		      :tex (let ((y-tex (first (yaksha:mesh-textures mesh))))
			     (if y-tex
				 (yaksha::texture-jungl-texture y-tex)
				 *backup-tex*))
		      :light-pos light-pos
		      :light-intensity 0.5)))))))

  ;; so the main loop can (hasty:run-pass *render-pass*)
  (setf *render-pass* (hasty:get-system 'renderable)))

;;----------------------------------------------------------------------
