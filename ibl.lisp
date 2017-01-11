(in-package :lark)

(defun-g hammersley-get-sample ((i :uint) (n :uint))
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (v! (/ (float i) (float n))
      (radical-inverse-vdc i)))

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


(defun-g iblggx-prefilter-env-map ((r :vec3) (roughness :float)
                                   (env-map :sampler-2d))
  (let* ((normal r)
         (view-dir r)
         (prefiltered-color (v3! 0))
         (num-samples (uint 1024))
         (total-weight 0s0))
    (for (i 0) (< i num-samples) (setf i (+ 1 i))
         (let* ((xi (hammersley-get-sample i num-samples))
                (h (importance-sample-ggx xi roughness normal))
                (l (- (* 2  (dot view-dir h) h)
                      view-dir))
                (n·l (saturate (dot normal l))))
           (%if (> n·l 0s0)
                (progn
                  (setf prefiltered-color
                       (+ prefiltered-color
                          (* (s~ (sample-equirectangular-tex env-map l) :xyz)
                             n·l)))
                  (setf total-weight (+ total-weight n·l))))))
    (if (> total-weight 0s0)
        (/ prefiltered-color total-weight)
        prefiltered-color)))

(defun-g iblggx-convolve-envmap ((tc :vec2) &uniform (env-map :sampler-2d)
                                 (roughness :float))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions tc)
    (values (iblggx-prefilter-env-map dir0 roughness env-map)
	    (iblggx-prefilter-env-map dir1 roughness env-map)
	    (iblggx-prefilter-env-map dir2 roughness env-map)
	    (iblggx-prefilter-env-map dir3 roughness env-map)
	    (iblggx-prefilter-env-map dir4 roughness env-map)
	    (iblggx-prefilter-env-map dir5 roughness env-map))))


(def-g-> iblggx-convolve-pass ()
  (pass-through-vert g-pt)
  (iblggx-convolve-envmap :vec2))
