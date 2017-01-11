;;----------------------------------------------------------------------
;; learn ibl

(defun-g learn-ibl-render-frag ((tc :vec2) &uniform
                                (albedo-sampler :sampler-2d)
                                (pos-sampler :sampler-2d)
                                (normal-sampler :sampler-2d)
                                (material-sampler :sampler-2d)
                                (irradiance-cube :sampler-cube)
                                (depth :sampler-2d))
  (let* ((world-pos (s~ (texture pos-sampler tc) :xyz))
         (normal (s~ (texture normal-sampler tc) :xyz))
	 (albedo (s~ (texture albedo-sampler tc) :xyz))
	 (view-dir (normalize (- world-pos)))
         (irradiance (s~ (texture irradiance-cube normal) :xyz))
         (ambient (v3! 0.0004))
         (diffuse (* albedo (+ ambient irradiance))))

    ;; set the depth so the skybox works
    (setf gl-frag-depth (x (texture depth tc)))

    ;; blort
    (tone-map-uncharted2 diffuse 2s0 1s0)
    ;;normal
    ))

(def-g-> learn-ibl-render-pass ()
  (pass-through-vert g-pt)
  (learn-ibl-render-frag :vec2))
