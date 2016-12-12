(in-package :lark)

(defun-g ggx-distribution ((n·h :float) (m :float))
  (let* ((m² (* m m))
	 (f (+ 1.0 (* (- (* n·h m²)
			 n·h)
		      n·h)))
	 (f² (* f f)))
    ;; we don't divide by +pi+ here as we will do that in the main brdf function
    (/ m² f²)))

;; epic's technique
(defun-g importance-sample-ggx ((xi :vec2) (roughness :float) (n :vec3))
  (let* ((α (* roughness roughness))
	 (φ (* 2 +pi+ (x xi)))
	 (cosθ (sqrt (/ (- 1 (y xi))
			(+ 1 (* (- (* α α) 1) (y xi))))))
	 (sinθ (sqrt (- 1 (* cosθ cosθ))))
	 (h (v! (* sinθ (cos φ))
		(* sinθ (sin φ))
		cosθ))
	 (up-vector (if (< (abs (z n)) 0.999)
			(v! 0 0 1)
			(v! 1 0 0)))
	 (tangent-x (normalize (cross up-vector n)))
	 (tangent-y (cross n tangent-x)))
    ;; apparently tangent to world space
    (+ (* tangent-x (x h))
       (* tangent-y (y h))
       (* n (z h)))))

(defun-g integrate-specular-cube-ld ((v :vec3) (n :vec3) (roughness :float)
				     (value-multiplier :float)
				     (cube :sampler-cube)
				     (resolution :int))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0))
	(bdrf-accum-weight 0s0))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let* ((eta (get-sample i sample-count))
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
