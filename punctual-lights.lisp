(in-package :lark)

;;----------------------------------------------------------------------

(defun-g ggx-distribution ((n·h :float) (m :float))
  (let* ((m² (* m m))
	 (f (+ 1.0 (* (- (* n·h m²)
			 n·h)
		      n·h)))
	 (f² (* f f)))
    ;; we don't divide by +pi+ here as we will do that in the main brdf function
    (/ m² f²)))

(defun-g specular-brdf ((n·v :float) (l·h :float) (n·h :float) (n·l :float)
			(h :vec3) (f0 :vec3) (f90 :float) (roughness :float))
  (let ((f (fresnel-schlick f0 f90 l·h))
	(g (ggx-geom-smith-correlated n·v n·l roughness))
	(d (ggx-distribution n·h roughness)))
    (/ (* d g f) +pi+)))

(defun-g punctual-light ((albedo :vec3) (n·v :float) (half-vec :vec3)
			 (l·h :float) (n·h :float) (n·l :float)
			 (linear-roughness :float) (roughness :float)
			 (metallic :float))
  (let* ((fd (/ (disney-diffuse n·v n·l l·h linear-roughness) +pi+))
	 (f0 (mix (v3! 0.04) albedo metallic))
	 (f90 (saturate (* 50s0 (dot f0 (v3! 0.33)))))
	 (fr (specular-brdf n·v l·h n·h n·l half-vec f0 f90 roughness))
	 (brdf (+ (* albedo fd) fr)))
    (* brdf n·l
       ;;attenuation
       ;;light-color
       )))

(defun-g some-shit-frag ((tc :vec2) &uniform
			 (albedo-sampler :sampler-2d)
			 (pos-sampler :sampler-2d)
			 (normal-sampler :sampler-2d)
			 (material-sampler :sampler-2d)
			 (light-pos :vec3)
			 (diffuse-lp-cube :sampler-cube)
			 (specular-lp-cube :sampler-cube) (dfg :sampler-2d)
			 (depth :sampler-2d)
			 (eq-rec :sampler-2d))
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
	 (reflect-vec (normalize (- (* 2 normal (dot normal view-dir))
				    view-dir)))
	 (plight (punctual-light albedo n·v half-vec
				l·h n·h n·l
				linear-roughness
				roughness
				metallic))
	 (final (+ plight
		   ;; ibl eventually :)
                   )))

    ;; set the depth so the skybox works
    (setf gl-frag-depth (x (texture depth tc)))

    ;;(pow final (v3! (/ 1 2.2)))
    (tone-map-uncharted2 final 2s0 1s0)
    ;;albedo
    ;;(v3! metallic)
    ;;normal
    ))

(def-g-> some-shit-pass ()
  (pass-through-vert g-pt)
  (some-shit-frag :vec2))

;;----------------------------------------------------------------------
