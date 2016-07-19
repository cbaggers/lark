(in-package :lark)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun-g pack-gbuffer-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (let* ((pos (* (pos vert) 3))
	 (m->w (m4:to-mat3 (get-transform model-space *world-space*)))

	 (normal (normalize (* m->w (yaksha:normal vert))))
	 (up-vec (v! 0 1 0))

	 (tangent (normalize (cross normal up-vec)))
	 (bitangent (normalize (cross normal tangent)))

	 (btn-mat (m! tangent bitangent normal)))

    (values (in *clip-space*
	      (in model-space (sv! pos 1.0)))
	    (in *world-space*
	      (in model-space (sv! pos 1.0)))
	    btn-mat
	    (yaksha:uv vert)
	    normal)))


(defun-g pack-gbuffer-frag ((world-pos :vec4) (btn-mat :mat3) (uv :vec2) (misc :vec3)
			    &uniform (base-tex :sampler-2d)
			    (norm-tex :sampler-2d)
			    (mat-tex :sampler-2d)
			    (metallic-tex :sampler-2d)
			    (roughness-tex :sampler-2d))
  (let* ((met (pow (x (texture metallic-tex uv)) 2.2))
	 (rough (pow (x (texture roughness-tex uv)) 2.2))
	 (ts-norm (- (* (s~ (texture norm-tex uv) :xyz) 2) (v3! 1)))
	 (wnormal (normalize (* btn-mat ts-norm))))
    (values world-pos
	    wnormal
	    (s~ (texture base-tex uv) :xyz)
	    (v! met rough 0))))


(def-g-> pack-gbuffer-pass ()
  #'pack-gbuffer-vert #'(pack-gbuffer-frag :vec4 :mat3 :vec2 :vec3))

;;----------------------------------------------------------------------

(defun-g fresnel-schlick ((f0 :vec3) (f90 :float) (u :float))
  (+ f0
     (* (- (v3! f90) f0)
	(pow (- 1s0 u) 5s0))))

;;----------------------------------------------------------------------

(defun-g disney-diffuse ((n·v :float) (n·l :float) (l·h :float)
			 (linear-roughness :float))
  ;; with renormalization of it's energy
  (let* ((energy-bias (mix 0.0 0.5 linear-roughness))
	 (energy-factor (mix 1.0 (/ 1.0 1.51) linear-roughness))
	 (l·h² (* l·h l·h))
	 (fd90 (+ energy-bias (* 2.0 l·h² linear-roughness)))
	 (f0 (v! 1 1 1))
	 (light-scatter (x (fresnel-schlick f0 fd90 n·l)))
	 (view-scatter (x (fresnel-schlick f0 fd90 n·v))))
    (* light-scatter view-scatter energy-factor)))

;;----------------------------------------------------------------------

(defun-g blinn-diffuse ((normal :vec3) (light-dir :vec3) (base-color :vec3))
  (max 0s0 (dot normal light-dir)))

(defun-g some-shit-frag ((tc :vec2) &uniform
			 (albedo-sampler :sampler-2d)
			 (pos-sampler :sampler-2d)
			 (normal-sampler :sampler-2d)
			 (material-sampler :sampler-2d)
			 (light-pos :vec3))
  (let* ((world-pos (s~ (texture pos-sampler tc) :xyz))
	 (light-dir (normalize (- light-pos world-pos)))
	 (normal (s~ (texture normal-sampler tc) :xyz))
	 (albedo (s~ (texture albedo-sampler tc) :xyz))
	 (view-dir (normalize (- world-pos)))

	 (n·v (+ (abs (dot normal view-dir)) 0.00001s0))
	 (half-vec (normalize (+ view-dir light-dir)))
	 (l·h (saturate (dot light-dir half-vec)))
	 (n·h (saturate (dot normal half-vec)))
	 (n·l (saturate (dot normal light-dir)))

	 ;; is linear value (we think)
	 (metallic (x (texture material-sampler tc)))
	 (roughness (y (texture material-sampler tc)))
	 ;; perceptualy linear roughness (α)
	 (linear-roughness (* roughness roughness))
	 ;;(lin-col (* albedo (blinn-diffuse normal light-dir albedo)))
	 (lin-col (* albedo (/ (disney-diffuse n·v n·l l·h linear-roughness) +pi+)))
	 )
    (pow lin-col (v3! (/ 1 2.2)))))

(def-g-> some-shit-pass ()
  #'pass-through-vert #'some-shit-frag)

;;----------------------------------------------------------------------

(defun render-thing (thing camera render-state)
  (with-slots (light-probe-diffuse light-probe-specular gbuffer dfg)
      render-state
    (using-camera camera
      (loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	 (with-fbo-bound ((fbo gbuffer))
	   (map-g #'pack-gbuffer-pass (yaksha:mesh-stream mesh)
		  :model-space (model-space thing)
		  :base-tex (base-sampler thing)
		  :norm-tex (normal-sampler thing)
		  :metallic-tex (metallic-sampler thing)
		  :roughness-tex (roughness-sampler thing)))))))

;;----------------------------------------------------------------------

(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe-diffuse light-probe-specular env-map
		     gbuffer)
	render-state
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (clear-fbo (fbo gbuffer))

      (map nil λ(render-thing (update-thing _) camera render-state)
	   (things *game-state*))

      (using-camera camera
	(map-g #'some-shit-pass *quad-stream*
	       :pos-sampler (pos-sampler gbuffer)
	       :albedo-sampler (base-sampler gbuffer )
	       :normal-sampler (norm-sampler gbuffer)
	       :material-sampler (mat-sampler gbuffer)
	       :light-pos (v! 0 1000 -160)))
      (swap))))


;;----------------------------------------------------------------------
