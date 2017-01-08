(in-package :lark)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun-g pack-gbuffer-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (let* ((pos (* (pos vert) 3.9))
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
  (let* ((met (pow (x (texture metallic-tex uv)) 2.2))    ;; [TODO] remove these pows and load in in srgb
	 (rough (pow (x (texture roughness-tex uv)) 2.2))
	 (ts-norm (- (* (s~ (texture norm-tex uv) :xyz) 2) (v3! 1)))
	 (wnormal (normalize (* btn-mat ts-norm))))
    (values world-pos
	    wnormal
	    (s~ (texture base-tex uv) :xyz)
	    ;;(nineveh::mipmap-level->grey base-tex uv)
	    (v! met rough 0))))


(def-g-> pack-gbuffer-pass ()
  (pack-gbuffer-vert yaksha:vertex)
  (pack-gbuffer-frag :vec4 :mat3 :vec2 :vec3))

;;----------------------------------------------------------------------

(defun-g specular-brdf ((n·v :float) (l·h :float) (n·h :float) (n·l :float)
			(h :vec3) (f0 :vec3) (f90 :float) (roughness :float))
  (let ((f (fresnel-schlick f0 f90 l·h))
	(g (ggx-geom-smith-correlated n·v n·l roughness))
	(d (ggx-distribution n·h roughness)))
    (/ (* d g f) +pi+)))

;;----------------------------------------------------------------------

(defun-g linear-roughness-to-mip-level ((linear-roughness :float)
					(mip-count :int))
  (* (sqrt linear-roughness) mip-count))


(defun-g get-specular-dominant-dir ((n :vec3) (r :vec3) (roughness :float))
  (let* ((smoothness (saturate (- 1 roughness)))
	 (lerp-factor (* smoothness (+ (sqrt smoothness) roughness))))
    (mix n r lerp-factor)))


(defun-g evaluate-ibl-specular ((n :vec3) (r :vec3) (n·v :float)
				(linear-roughness :float) (roughness :float)
				(f0 :vec3) (f90 :float) (cube :sampler-cube)
				(ld-mip-max-level :int) (dfg :sampler-2d))
  (let* ((dominant-r (get-specular-dominant-dir n r roughness))
	 (dfg-tex-size 128)
	 ;; Rebuild the function
	 ;; L . D. ( f0.Gv.(1-Fc) + Gv.Fc ) . cosTheta / (4 . NdotL . NdotV)
	 (n.v (max n·v (/ 0.5 dfg-tex-size)))
	 (mip-level (linear-roughness-to-mip-level linear-roughness
						   ld-mip-max-level))
	 (pre-ld (texture-lod cube dominant-r mip-level))
	 (pre-dfg (s~ (texture dfg (v! n·v roughness)) :xy)))
    ;; usually the following is (v! .. (w pre-ld)) but we arent using alpha
    (* (+ (* f0 (x pre-dfg))
	  (* (v3! f90) (y pre-dfg)))
       (s~ pre-ld :xyz))))


(defun-g get-diffuse-dominant-dir ((n :vec3) (v :vec3) (n·v :float)
				   (roughness :float))
  (let* ((a (- (* 1.02341 roughness) 1.51174))
	 (b (+ (* -0.511705 roughness) 0.755868))
	 (lerp-factor (saturate (* (+ (* n·v a) b) roughness))))
    (mix n v lerp-factor)))


(defun-g evaluate-ibl-diffuse ((n :vec3) (v :vec3) (n·v :float)
			       (roughness :float) (lp-cube :sampler-cube)
			       (dfg :sampler-2d))
  (let* ((dominant-n (get-diffuse-dominant-dir n v n·v roughness))
	 (diffuse-lighting (texture lp-cube dominant-n))
	 (diff-f (z (texture dfg (v! n·v roughness)))))
    ;; as with specular, we arent using alpha
    (* (s~ diffuse-lighting :xyz) diff-f)))

;; I've been messing with this
(defun-g evaluate-ibl ((wnormal :vec3) (wview-dir :vec3) (n·v :float)
		       (roughness :float) (linear-roughness :float)
		       (diffuse-lp-cube :sampler-cube)
		       (specular-lp-cube :sampler-cube) (dfg :sampler-2d)
		       (base-color :vec3) (metallic :float)
		       (reflect-vec :vec3)
		       (eq-rec :sampler-2d))
  (let* ((albedo (mix base-color (v3! 0) metallic))

	 ;; lighting time
	 (fd (evaluate-ibl-diffuse wnormal wview-dir n·v roughness
				   diffuse-lp-cube dfg))
	 (fr (let* (;; again stole these two from the analytical light
	 	    (f0 (mix (v3! 0.04) base-color metallic))
	 	    (f90 (saturate (* 50s0 (dot f0 (v3! 0.33))))))
	       ;; the 7 is the num of mipmaps, I shoved that there as I dont
	       ;; know what I'm doing
	       (evaluate-ibl-specular wnormal reflect-vec n·v
				      linear-roughness roughness
	 			      f0 f90 specular-lp-cube 7 dfg)))
	 )
    ;; again taken from analytical. who knows what to do here?
    (+ (* albedo fd)
       ;;fr
       )))

;;----------------------------------------------------------------------

(defun-g blinn-diffuse ((normal :vec3) (light-dir :vec3) (base-color :vec3))
  (max 0s0 (dot normal light-dir)))

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
	 (ibl (evaluate-ibl
	       normal view-dir n·v roughness
	       linear-roughness
	       diffuse-lp-cube specular-lp-cube dfg
	       albedo metallic reflect-vec
	       eq-rec))
	 (plight (punctual-light albedo n·v half-vec
				l·h n·h n·l
				linear-roughness
				roughness
				metallic))
	 (final (+ plight
		   ;;ibl
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
;; learn ibl

(defun-g calc-irr ((normal :vec3) (env-map :sampler-2d))
  (let* ((up (v! 0 1 0))
         (right (normalize (cross up normal)))
         (up (cross normal right))
         (sampled-color (v! 0 0 0))
         (index 0s0))
    ;; loop over hemisphere
    (for (φ 0s0) (< φ 6.283) (setq φ (+ φ 0.025))
         (for (θ 0s0) (< θ 1.57) (setq θ (+ θ 0.1))
              (let* ((temp (+ (* (cos φ) right)
                              (* (sin φ) up)))
                     (sample-vec (+ (* normal (cos θ))
                                    (* temp (sin θ))))
                     ;; change sample-vec in the form below to normal to
                     ;; prove that both uv->cube-map-directions and
                     ;; sample-equirectangular-tex are working.
                     (sample (s~ (sample-equirectangular-tex
                                  env-map sample-vec)
                                 :xyz))
                     (final (* sample (cos θ) (sin θ))))
                (setf sampled-color (+ sampled-color final))
                (setf index (+ index 1)))))
    (v! (/ (* sampled-color +pi+) index) 1)))

#+t
(setf *regen-light-probe* t)

(defun-g learn-ibl-convolve-envmap ((tc :vec2) &uniform (env-map :sampler-2d))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions tc)
    (values (calc-irr dir0 env-map)
	    (calc-irr dir1 env-map)
	    (calc-irr dir2 env-map)
	    (calc-irr dir3 env-map)
	    (calc-irr dir4 env-map)
	    (calc-irr dir5 env-map))))

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

(def-g-> learn-ibl-convolve-pass ()
  (pass-through-vert g-pt)
  (learn-ibl-convolve-envmap :vec2))

;;----------------------------------------------------------------------

(defvar *regen-light-probe* nil)

(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe-diffuse light-probe-specular env-map
		     gbuffer)
	render-state
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      ;; Failed IBL
      ;;
      ;; ;; populate the dfg LUT
      ;; (clear-fbo (fbo dfg))
      ;; (map-g-into (fbo dfg)
      ;; 		  #'dfg-texture-pass *quad-stream*)
      ;;
      ;; ;; precalc the diffuse portion of the IBL
      ;; (clear-fbo (fbo light-probe-diffuse))
      ;; ;; (map-g-into (fbo light-probe-diffuse)
      ;; ;; 		  #'diffuse-sample-hdr-cube *quad-stream*
      ;; ;; 		  :value-multiplier 1s0 :cube env-map)
      ;; (map-g-into (fbo light-probe-diffuse)
      ;; 		  #'diffuse-sample-hdr-2d *quad-stream*
      ;; 		  :value-multiplier 1s0 :tex *catwalk*)
      ;;
      ;; (generate-mipmaps (cube light-probe-diffuse))
      ;;
      ;; ;; precalc the specular portion of the IBL
      ;; (clear-fbo (fbo light-probe-specular))
      ;; (map-g-into (fbo light-probe-specular)
      ;; 		  #'specular-sample-hdr-cube *quad-stream*
      ;; 		  :value-multiplier 1s0 :cube env-map :roughness 0.1)
      ;;
      ;; (generate-mipmaps (cube light-probe-specular))

      (when *regen-light-probe*
        (setf *regen-light-probe* nil)

        (clear-fbo (fbo light-probe-diffuse))

        (map-g-into (fbo light-probe-diffuse)
                    #'learn-ibl-convolve-pass *quad-stream*
                    :env-map *catwalk*))

      ;;
      (clear-fbo (fbo gbuffer))

      ;; populate the gbuffer
      (map nil λ(render-thing (update-thing _) camera render-state)
      	   (things *game-state*))

      ;; deferred pass
      (using-camera camera
        (map-g #'learn-ibl-render-pass *quad-stream*
               :pos-sampler (pos-sampler gbuffer)
               :albedo-sampler (base-sampler gbuffer)
               :normal-sampler (norm-sampler gbuffer)
               :material-sampler (mat-sampler gbuffer)
               :irradiance-cube (sampler light-probe-diffuse)
      	       :depth (depth-sampler gbuffer)))

      ;; (using-camera camera
      ;;   (map-g #'some-shit-pass *quad-stream*
      ;;          :pos-sampler (pos-sampler gbuffer)
      ;;          :albedo-sampler (base-sampler gbuffer)
      ;;          :normal-sampler (norm-sampler gbuffer)
      ;;          :material-sampler (mat-sampler gbuffer)
      ;;          :light-pos (v! 0 1000 -0)
      ;;          :diffuse-lp-cube (sampler light-probe-diffuse)
      ;; 	       :specular-lp-cube (sampler light-probe-specular)
      ;; 	       :dfg (sampler dfg)
      ;; 	       :depth (depth-sampler gbuffer)
      ;;          :eq-rec *catwalk*))

      (render-sky camera render-state)
      (swap))))


;;----------------------------------------------------------------------
