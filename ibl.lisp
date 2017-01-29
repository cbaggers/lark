(in-package :lark)

(defun-g hammersley-get-sample ((i :uint) (n :uint))
  ;; rename to hammersley-2d
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
  (let* ((normal r)   ;; ← this is why we lose the
         (view-dir r) ;; ← stretched reflections right?
         (prefiltered-color (v3! 0))
         (num-samples (uint 1024))
         (total-weight 0s0))
    (for (i 0) (< i num-samples) (setf i (+ 1 i))
         (let* ((xi (hammersley-get-sample i num-samples))
                (h (importance-sample-ggx xi roughness normal))
                (l (- (* 2  (dot view-dir h) h)
                      view-dir))
                (n·l (saturate (dot normal l))))
           (if (> n·l 0s0)
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

;;----------------------------------------------------------------------

"i is the i'th element of the series and n is the length of the series"
(defun-g hammersley-hemisphere-uniform ((i :uint) (n :uint))
  (let* ((ham-sample (hammersley-get-sample i n))
         (u (x ham-sample))
         (v (y ham-sample))
         (φ (* v 2s0 +pi+))
         (cosθ (- 1s0 u))
         (sinθ (sqrt (- 1 (* cosθ cosθ)))))
    (v! (* (cos φ) sinθ)
        (* (sin φ) sinθ)
        cosθ)))

(defun-g ibl-diffuse-filter-env-map ((normal :vec3) (roughness :float)
                                     (env-map :sampler-2d))
  (let* ((up-vector (if (< (abs (z normal)) 0.999)
			(v! 0 0 1)
			(v! 1 0 0)))
	 (tangent-x (normalize (cross up-vector normal)))
	 (tangent-y (cross normal tangent-x))
         (accum-col (v3! 0))
         (num-samples (uint 1024)))
    (for (i 0) (< i num-samples) (setf i (+ 1 i))
         (let* ((ham-dir (hammersley-hemisphere-uniform i num-samples))
                (sample-dir (+ (* tangent-x (x ham-dir))
                               (* tangent-y (y ham-dir))
                               (* normal (z ham-dir))))
                (col (s~ (sample-equirectangular-tex env-map sample-dir)
                         :xyz)))
           (setf accum-col (+ accum-col col))))
    (v! (/ (* accum-col (v3! +pi+))
           (* 10 num-samples))
        1)))

(defun-g ibl-diffuse-envmap ((tc :vec2) &uniform (env-map :sampler-2d)
                                 (roughness :float))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions tc)
    (values (ibl-diffuse-filter-env-map dir0 roughness env-map)
	    (ibl-diffuse-filter-env-map dir1 roughness env-map)
	    (ibl-diffuse-filter-env-map dir2 roughness env-map)
	    (ibl-diffuse-filter-env-map dir3 roughness env-map)
	    (ibl-diffuse-filter-env-map dir4 roughness env-map)
	    (ibl-diffuse-filter-env-map dir5 roughness env-map))))

(def-g-> ibl-diffuse-pass ()
  (pass-through-vert g-pt)
  (ibl-diffuse-envmap :vec2))

;;----------------------------------------------------------------------

(defun-g integrate-brdf ((roughness :float) (n·v :float))
  (let ((view-dir (v! (- 1s0 (* n·v n·v)) ;;sin
                      0
                      n·v));; cos
        (normal (v! 0 0 1))
        (num-samples 1024)
        (a 0s0)
        (b 0s0))
    (for (i 0) (< i num-samples) (setf i (+ i 1))
         (let* ((ham-sample (hammersley-get-sample i num-samples))
                (h (importance-sample-ggx ham-sample roughness normal))
                (l (- (* 2 (dot view-dir h) h)
                      view-dir))
                (n·l (saturate (z l)))
                (n·h (saturate (z h)))
                (v·h (saturate (dot view-dir h))))
           (if (> n·l 0s0)
                (let* ((g (g-smith roughness n·v n·l))
                       (g-vis (/ (* g v·h) (* n·h n·v)))
                       (fc (expt (- 1 v·h) 5)))
                  (setf a (+ a (* (- 1 fc) g-vis)))
                  (setf b (+ b (* fc g-vis)))))))
    (/ (v! a b) num-samples)))

(defun-g compute-df-lut-frag ((tc :vec2))
  (integrate-brdf (x tc) (y tc)))

(def-g-> compute-dfg-lut-pass ()
  (pass-through-vert g-pt)
  (compute-df-lut-frag :vec2))
