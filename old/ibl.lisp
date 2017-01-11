(defun-g integrate-diffuse-cube-ld ((n :vec3) (value-multiplier :float)
				    (cube :sampler-cube))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0)))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let ((eta (hammersley-get-sample i sample-count)))
	   (multiple-value-bind (l n·l pdf)
	       (importance-sample-cos-dir eta n)
	     (%if (> n·l 0s0)
		  (let ((color (* (texture-lod cube l 0)
				  value-multiplier)))
		    (incf bdrf-accum color))))))
    (* bdrf-accum (/ 1s0 sample-count))))

(defun-g ld-diffuse-texture-cube ((uv :vec2) &uniform (value-multiplier :float)
				  (cube :sampler-cube))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions uv)
    (values (integrate-diffuse-cube-ld dir0 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir1 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir2 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir3 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir4 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir5 value-multiplier cube))))


(def-g-> diffuse-sample-hdr-cube ()
  (pass-through-vert g-pt)
  (ld-diffuse-texture-cube :vec2))


(defun-g integrate-specular-cube-ld ((v :vec3) (n :vec3) (roughness :float)
				     (value-multiplier :float)
				     (cube :sampler-cube)
				     (resolution :int))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0))
	(bdrf-accum-weight 0s0))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let* ((eta (hammersley-get-sample i sample-count))
		(h (importance-sample-ggx eta roughness n))
		(l (- (* 2 (dot v h) h) v))
		(n·l (dot n l)))
	   (%if (> n·l 0s0)
		;; Use pre-filtered importance sampling (i.e use lower mipmap
		;; level for fetching sample with low probability in order
		;; to reduce the variance).
		;; (Reference: GPU Gem3)
		;;
		;; Since we pre-integrate the result for normal direction,
		;; N == V and then NdotH == LdotH. This is why the BRDF pdf
		;; can be simplifed from:
		;;      pdf = D_GGX_Divide_Pi(NdotH, roughness)*NdotH/(4*LdotH);
		;; to
		;;      pdf = D_GGX_Divide_Pi(NdotH, roughness) / 4; ;;
		;; The mipmap level is clamped to something lower than 8x8
		;; in order to avoid cubemap filtering issues.
		;;
		;; - OmegaS: Solid angle associated with a sample
		;; - OmegaP: Solid angle associated with a pixel of the cubemap.
		(let* ((n·h (saturate (dot n h)))
		       (l·h (saturate (dot l h)))
		       (pdf (* (ggx-distribution n·h roughness) (/ +inv-pi+ 4)))
		       (Ωs (/ 1s0 (* sample-count pdf)))
		       (Ωp (* 4s0 (/ +pi+ (* 6 resolution resolution))))
		       (sample-mipmap-level
			(float (clamp (* 0.5s0 (float (log2 (/ Ωs Ωp)))) 0 8)))
		       (li (* (texture-lod cube l sample-mipmap-level)
			      value-multiplier)))
		  (incf bdrf-accum (* li n·l))
		  (incf bdrf-accum-weight n·l)))))
    (* bdrf-accum (/ 1s0 bdrf-accum-weight))))



(defun-g ld-specular-texture ((uv :vec2) &uniform (value-multiplier :float)
			      (cube :sampler-cube) (roughness :float))
  (let ((resolution 128))
    (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
	(uv->cube-map-directions uv)
      (values
       (integrate-specular-cube-ld
	dir0 dir0 roughness value-multiplier cube resolution)
       (integrate-specular-cube-ld
	dir1 dir1 roughness value-multiplier cube resolution)
       (integrate-specular-cube-ld
	dir2 dir2 roughness value-multiplier cube resolution)
       (integrate-specular-cube-ld
	dir3 dir3 roughness value-multiplier cube resolution)
       (integrate-specular-cube-ld
	dir4 dir4 roughness value-multiplier cube resolution)
       (integrate-specular-cube-ld
	dir5 dir5 roughness value-multiplier cube resolution)))))


(def-g-> specular-sample-hdr-cube ()
  (pass-through-vert g-pt)
  (ld-specular-texture :vec2))

(defun-g importance-sample-cos-dir ((u :vec2) (n :vec3))
  (let* ((up-vec (if (< (abs (z n)) 0.999)
		     (v! 0 0 1)
		     (v! 1 0 0)))
	 (tangent-x (normalize (cross up-vec n)))
	 (tangent-y (cross n tangent-x))
	 (u1 (x u))
	 (u2 (y u))
	 (r (sqrt u1))
	 (φ (* u2 +pi+ 2))
	 (pre-l (v! (* r (cos φ))
		    (* r (sin φ))
		    (sqrt (max 0s0 (- 1s0 u1)))))
	 (l (normalize (+ (* tangent-x (y pre-l))
			  (* tangent-y (x pre-l))
			  (* n (z pre-l)))))
	 (n·l (dot l n))
	 (pdf (* n·l +inv-pi+)))
    (values l n·l pdf)))

(defun-g integrate-diffuse-2d-ld ((n :vec3) (value-multiplier :float)
				  (tex :sampler-2d))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0)))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let ((eta (hammersley-get-sample i sample-count)))
	   (multiple-value-bind (l n·l pdf)
	       (importance-sample-cos-dir eta n)
	     (%if (> n·l 0s0)
		  (let ((color (* (sample-equirectangular-tex tex l);; lod=0?
				  value-multiplier)))
		    (incf bdrf-accum color))))))
    (* bdrf-accum (/ 1s0 sample-count))))


(defun-g ld-diffuse-texture-2d ((uv :vec2) &uniform (value-multiplier :float)
				(tex :sampler-2d))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions uv)
    (values (integrate-diffuse-2d-ld dir0 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir1 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir2 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir3 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir4 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir5 value-multiplier tex))))

(def-g-> diffuse-sample-hdr-2d ()
  (pass-through-vert g-pt)
  (ld-diffuse-texture-2d :vec2))




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


(defun-g get-diffuse-dominant-dir ((n :vec3) (v :vec3) (n·v :float)
				   (roughness :float))
  (let* ((a (- (* 1.02341 roughness) 1.51174))
	 (b (+ (* -0.511705 roughness) 0.755868))
	 (lerp-factor (saturate (* (+ (* n·v a) b) roughness))))
    (mix n v lerp-factor)))


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
