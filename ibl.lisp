(in-package :lark)

;;----------------------------------------------------------------------

;; epic's technique
(defun-g importance-sample-ggx ((xi :vec2)
                                (roughness :float)
                                (normal :vec3)) ;; n
  (let* ((α (* roughness roughness))
         (φ (* 2 pi-f (x xi)))
         (cosθ (sqrt (/ (- 1f0 (y xi))
                        (+ 1f0 (* (- (* α α) 1f0)
                                  (y xi))))))
         (sinθ (sqrt (- 1 (* cosθ cosθ))))
         ;;
         ;; from spherical to cartesian coords
         (h (v! (* (cos φ) sinθ)
                (* (sin φ) sinθ)
                cosθ))
         ;;
         ;; from tangent-space vector to world-space sample vector
         (up-vector (if (< (abs (z normal)) 0.999)
                        (v! 0 0 1)
                        (v! 1 0 0)))
         (tangent (normalize (cross up-vector normal)))
         (bitangent (cross normal tangent)))
    ;; final
    (normalize
     (+ (* tangent (x h))
        (* bitangent (y h))
        (* normal (z h))))))


;;----------------------------------------------------------------------

(defun-g iblggx-prefilter-env-map ((sample-dir :vec3) ;; r
                                   (roughness :float)
                                   (env-map :sampler-2d))
  (let* ((normal sample-dir)   ;; ← this is why we lose the
         (view-dir sample-dir) ;; ← stretched reflections.
         (prefiltered-color (v3! 0))
         (num-samples (uint 1024))
         (total-weight 0s0))

    (for (i (uint 0)) (< i num-samples) (++ i)
         (let* ((xi (hammersley-nth-2d num-samples i))
                (half-vec (importance-sample-ggx xi roughness normal))
                (light-dir (normalize
                            (- (* 2 (dot view-dir half-vec) half-vec)
                               view-dir)))
                (n·l (saturate (dot normal light-dir))))
           (when (> n·l 0s0)
             (incf prefiltered-color
                   (* (s~ (sample-equirectangular-tex env-map light-dir) :xyz)
                      n·l))
             (incf total-weight n·l))))
    (v! (/ prefiltered-color total-weight) 1f0)))

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

(defun-g dfg-lookup ((dfg-lut :sampler-2d) (roughness :float) (n·v :float))
  (s~ (texture dfg-lut (v! n·v roughness)) :xy))

(defun-g integrate-brdf ((n·v :float) (roughness :float))
  (let ((view-dir (v! (sqrt (- 1s0 (* n·v n·v))) ;;sin
                      0
                      n·v));; cos
        ;;
        (a 0f0)
        (b 0f0)
        ;;
        (normal (v! 0f0 0f0 1f0))
        ;;
        (num-samples (uint 1024)))

    (for (i (uint 0)) (< i num-samples) (++ i)
         (let* ((ham-sample (hammersley-nth-2d num-samples i))
                (h (importance-sample-ggx ham-sample roughness normal))
                (l (normalize (- (* 2 (dot view-dir h) h)
                                 view-dir)))
                (n·l (saturate (z l)))
                (n·h (saturate (z h)))
                (v·h (saturate (dot view-dir h))))
           (when (> n·l 0f0)
             (let* ((remapped-α (α-remap-ibl roughness))
                    (g (geometry-smith n·v n·l remapped-α))
                    (g-vis (/ (* g v·h) (* n·h n·v)))
                    (fc (expt (- 1 v·h) 5)))
               (incf a (* (- 1 fc) g-vis))
               (incf b (* fc g-vis))))))
    (/ (v! a b) num-samples)))

(defun-g compute-df-lut-frag ((tc :vec2))
  (integrate-brdf (x tc) (y tc)))

(def-g-> compute-dfg-lut-pass ()
  (pass-through-vert g-pt)
  (compute-df-lut-frag :vec2))

;;----------------------------------------------------------------------

(defun-g select-ld-mipmap ((roughness :float))
  (* (float (- +ibl-mipmap-count+ 1)) roughness))

(defun-g calc-ibl-logl ((dfg-lut :sampler-2d)
                        (prefiltered-env-map :sampler-cube)
                        (irradiance-map :sampler-2d)
                        (normal :vec3)
                        (view-dir :vec3)
                        (albedo :vec3)
                        (metallic :float)
                        (roughness :float)
                        (f0 :vec3))
  (let* ((n·v (saturate (dot normal view-dir)))
         (reflect-dir (reflect (- view-dir) normal))
         ;;
         (env-brdf (dfg-lookup dfg-lut roughness n·v))
         (irradiance (s~ (sample-equirectangular-tex irradiance-map normal)
                         :xyz))
         (prefiltered-color (s~ (texture-lod prefiltered-env-map
                                             reflect-dir
                                             (select-ld-mipmap roughness))
                                :xyz))
         ;;
         (specular-component (fresnel-schlick-roughness
                              f0
                              (saturate (dot normal view-dir))
                              roughness))
         ;;
         (diffuse-component (* (- (v3! 1f0) specular-component) ;; kd
                               (- 1f0 metallic))) ;; ← metals have no diffuse
         (diffuse (* irradiance albedo))
         ;;
         (specular (* prefiltered-color (+ (* specular-component (x env-brdf))
                                           (v3! (y env-brdf)))))
         (ao 1f0)
         (ambient (* (+ (* diffuse diffuse-component)
                        specular)
                     ao)))
    ambient))

;;----------------------------------------------------------------------
