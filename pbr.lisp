(in-package :lark)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------
;; render with ibl

(defconstant +ibl-mipmap-count+ 5)

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
       (+ (* specular-color (x env-brdf))
          (v3! (y env-brdf))))))

(defun-g ibl-render-frag ((tc :vec2) &uniform
                          (albedo-sampler :sampler-2d)
                          (pos-sampler :sampler-2d)
                          (normal-sampler :sampler-2d)
                          (material-sampler :sampler-2d)
                          (specular-cube :sampler-cube)
                          (irradiance-cube :sampler-cube)
                          (dfg-lut :sampler-2d)
                          (depth :sampler-2d))
  ;;
  ;; Setup
  (let* ((world-pos (s~ (texture pos-sampler tc) :xyz))
         (normal (s~ (texture normal-sampler tc) :xyz))
	 (albedo (s~ (texture albedo-sampler tc) :xyz))
	 (view-dir (normalize (- world-pos)))
         (material (texture material-sampler tc))
         (metallic (x material))
	 (roughness (y material)))
    (setf gl-frag-depth (x (texture depth tc))) ;; set depth so the skybox works
    ;;
    ;; The Meat
    (let* ((n·v (saturate (dot normal view-dir)))
           (dfg-terms (dfg-lookup dfg-lut roughness n·v)) ;; also named env-brdf
           (irradiance (s~ (texture irradiance-cube normal)
                           :xyz))
           ;; f0: specular reflectence at normal incidence
           ;; f90: stolen from frostbite paper, probably not correct here but
           ;;      will do for now
           (f0 (mix (v3! 0.04) albedo metallic))
           (f90 (saturate (* 50s0 (dot f0 (v3! 0.33)))))
           (diffuse (s~ (+ (* f0 (x dfg-terms))
                           (v3! (* f90 (y dfg-terms)))
                           albedo)
                        :xyz))
           (specular (* diffuse (approximate-specular-ibl specular-cube dfg-lut
                                                          f0 roughness normal
                                                          view-dir dfg-terms)))
           (final (mix (* diffuse irradiance)
                       specular
                       metallic)))
      ;;
      ;; tonemap and we are done :)
      (tone-map-uncharted2 final 2s0 1s0))))

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

;;----------------------------------------------------------------------

(defvar *regen-light-probe* nil)

(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe-diffuse light-probe-specular env-map
		     gbuffer)
	render-state
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (when *regen-light-probe*
        (setf *regen-light-probe* nil)

        (clear-fbo (fbo light-probe-diffuse))

        (map-g-into (fbo light-probe-diffuse)
                    #'iblggx-convolve-pass *quad-stream*
                    :env-map *catwalk*
                    :roughness 0.0))

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
      ;;   (map-g #'light-the-scene-pass *quad-stream*
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
