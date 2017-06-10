(in-package :lark)

(defun-g hammersley-get-sample ((i :uint) (n :uint))
  ;; rename to hammersley-2d
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (v! (/ (float i) (float n))
      (radical-inverse-vdc i)))

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


(defun-g iblggx-prefilter-env-map ((sample-dir :vec3) ;; r
                                   (roughness :float)
                                   (env-map :sampler-2d))
  (let* ((normal sample-dir)   ;; ← this is why we lose the
         (view-dir sample-dir) ;; ← stretched reflections.
         (num-samples (uint 1024))
         (total-weight 0s0)
         (prefiltered-color (v3! 0)))
    (for (i (uint 0)) (< i num-samples) (setf i (+ 1 i))
         (let* ((xi (hammersley-get-sample i num-samples))
                (h (importance-sample-ggx xi roughness normal))
                (l (normalize
                    (- (* 2 (dot view-dir h) h)
                       view-dir)))
                (n·l (saturate (dot normal l))))
           (when (> n·l 0s0)
             (incf prefiltered-color
                   (* (s~ (sample-equirectangular-tex env-map l) :xyz)
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
  (let* ((up-vector (v! 0 1 0))
         (tangent-x (normalize (cross up-vector normal)))
         (tangent-y (normalize (cross normal tangent-x)))
         (accum-col (v3! 0))
         (num-samples 1024)
         (mat (m3:from-columns tangent-x tangent-y normal)))
    (for (i 0) (< i num-samples) (setf i (+ 1 i))
         (let* ((ham-dir (hammersley-hemisphere-uniform i num-samples))
                (sample-dir (* mat ham-dir))
                (col (s~ (sample-equirectangular-tex env-map sample-dir)
                         :xyz))
                ;;(col sample-dir)
                )
           (setf accum-col (+ accum-col col))))
    (v! (/ accum-col num-samples)
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
         (let* ((ham-sample (hammersley-get-sample i num-samples))
                (h (importance-sample-ggx ham-sample roughness normal))
                (l (normalize (- (* 2 (dot view-dir h) h)
                                 view-dir)))
                (n·l (saturate (z l)))
                (n·h (saturate (z h)))
                (v·h (saturate (dot view-dir h))))
           (when (> n·l 0f0)
             (let* ((g (geometry-smith n·v n·l #'α-remap-ibl roughness))
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
  (mix (float 0) (- (float +ibl-mipmap-count+) 1) roughness))

(defun-g dfg-lookup ((dfg-lut :sampler-2d) (roughness :float) (n·v :float))
  (s~ (texture dfg-lut (v! roughness n·v)) :xy))

(defun-g approximate-specular-ibl ((specular-cube :sampler-cube)
                                   (dfg-lut :sampler-2d)
                                   (specular-color :vec3)
                                   (roughness :float)
                                   (normal :vec3)
                                   (view-dir :vec3)
                                   (env-brdf :vec2))
  (let* ((r (- (* 2 (dot normal view-dir) normal)
               view-dir))
         (mipmap (select-ld-mipmap roughness))
         (prefiltered-color (s~ (texture-lod specular-cube r mipmap)
                                :xyz)))
    (* prefiltered-color
       (+ (* specular-color
             (x env-brdf))
          (v3! (y env-brdf))))))

(defun-g calc-ibl ((dfg-lut :sampler-2d)
                   (specular-cube :sampler-cube)
                   (irradiance-map :sampler-2d)
                   (n·v :float)
                   (normal :vec3)
                   (view-dir :vec3)
                   (albedo :vec3)
                   (metallic :float)
                   (roughness :float)
                   (half-vec :vec3))
  (let* ((env-brdf (dfg-lookup dfg-lut roughness n·v)) ;; also named env-brdf
         (irradiance (s~ (sample-equirectangular-tex irradiance-map normal)
                         :xyz))
         ;; f0: specular reflectence at normal incidence
         ;; f90: stolen from frostbite paper, probably not correct here but
         ;;      will do for now
         (f0 (mix (v3! 0.04) albedo metallic) ;;(v3! 0.04)
           )
         (f90 (saturate (* 50s0 (dot f0 (v3! 0.33)))) ;;0.04
           )
         (fresnel (fresnel-schlick f0 (saturate (dot normal half-vec))))
         (diffuse (s~ (* (+ (* f0 (x env-brdf))
                            (v3! (* f90 (y env-brdf))))
                         albedo)
                      :xyz))
         (spec-approx (approximate-specular-ibl specular-cube
                                                dfg-lut
                                                fresnel
                                                roughness
                                                normal
                                                view-dir
                                                env-brdf))
         (specular (* diffuse spec-approx)))
    (mix (* diffuse irradiance)
         specular
         metallic)))

(defun-g calc-ibl-logl ((dfg-lut :sampler-2d)
                        (prefiltered-env-map :sampler-cube)
                        (irradiance-map :sampler-2d)
                        (n·v :float)
                        (normal :vec3)
                        (view-dir :vec3)
                        (albedo :vec3)
                        (metallic :float)
                        (roughness :float))
  (let* (
         ;;
         (f0 (simple-f0  albedo metallic))
         (specular-component (fresnel-schlick-roughness
                              f0
                              (saturate (dot normal view-dir))
                              roughness))
         ;;
         (diffuse-component (- (v3! 1f0) specular-component))
         (irradiance (s~ (sample-equirectangular-tex irradiance-map normal)
                         :xyz))
         (diffuse (* irradiance albedo))
         (ambient (* diffuse diffuse-component))
         ;;
         (specular (v! 0 0 0))
         )
    ambient
    specular))

;;----------------------------------------------------------------------
