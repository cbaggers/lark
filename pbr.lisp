(in-package :lark)
(in-readtable fn:fn-reader)g

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
