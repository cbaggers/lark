(in-package :lark)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------
;; render with ibl

(defun-g pbr-basic ((tc :vec2) &uniform
                    (cam-pos :vec3) ;; world-space
                    (light-pos :vec3) ;; world-space
                    (light-color :vec3)
                    (albedo-sampler :sampler-2d)
                    (pos-sampler :sampler-2d)
                    (normal-sampler :sampler-2d)
                    (material-sampler :sampler-2d)
                    (specular-cube :sampler-cube)
                    (irradiance-map :sampler-2d)
                    (dfg-lut :sampler-2d)
                    (depth :sampler-2d))
  ;;
  ;; Setup
  (let* (;; unpack deferred
         (world-pos (s~ (texture pos-sampler tc) :xyz))
         (normal (s~ (texture normal-sampler tc) :xyz))
         (albedo (s~ (texture albedo-sampler tc) :xyz))
         (material (texture material-sampler tc))
         (metallic (x material))
         (roughness (y material)))
    (setf gl-frag-depth (x (texture depth tc)))
    ;;
    (let* ((view-dir (normalize (- cam-pos world-pos)))
           ;;
           (f0 (simple-f0 albedo metallic))
           ;;
           (lₒ (cook-torrance-brdf world-pos
                                   normal
                                   albedo
                                   metallic
                                   roughness
                                   f0
                                   light-pos
                                   light-color
                                   view-dir))
           ;; ibl
           (ibl (calc-ibl dfg-lut specular-cube irradiance-map normal
                          view-dir albedo metallic roughness f0))
           ;;
           (final (+ ibl lₒ)))
      ;;
      (tone-map-linear final 0.5f0))))

(def-g-> light-the-scene-pass ()
  (pass-through-vert g-pt)
  (pbr-basic :vec2))

;;----------------------------------------------------------------------

(defvar *regen-light-probe* t)

(defun render (camera game-state)
  (let* ((render-state (game-state-render-state game-state)))
    (with-slots (dfg light-probe env-map gbuffer) render-state
      (when *regen-light-probe*
        (setf *regen-light-probe* nil)

        ;; regen the dfg-lut (only ever needs to be done once.. but meh)
        (clear-fbo (dfg-lookup-fbo dfg))
        (map-g-into (dfg-lookup-fbo dfg) #'compute-dfg-lut-pass *quad-stream*)

        ;; specular ggx
        (loop :for fbo :in (light-probe-specular-fbos light-probe) :for i :from 0 :do
           (clear-fbo fbo)
           (let* ((roughness (/ (float i 0f0) (- +ibl-mipmap-count+ 1))))
             (map-g-into fbo #'iblggx-convolve-pass *quad-stream*
                         :env-map *catwalk*
                         :roughness roughness))))

      ;;
      (clear-fbo (gbuffer-fbo gbuffer))

      ;; populate the gbuffer
      (map nil λ(render-thing (update-thing _) camera render-state)
           (game-state-things *game-state*))

      ;;draw & light
      (using-camera camera
        (map-g #'light-the-scene-pass *quad-stream*
               :pos-sampler (gbuffer-pos-sampler gbuffer)
               :albedo-sampler (gbuffer-base-sampler gbuffer)
               :normal-sampler (gbuffer-norm-sampler gbuffer)
               :material-sampler (gbuffer-mat-sampler gbuffer)
               :specular-cube (light-probe-specular-sampler light-probe)
               :irradiance-map *convolved-env*
               :dfg-lut (dfg-lookup-sampler dfg)
               :depth (gbuffer-depth-sampler gbuffer)
               :light-pos (v! -80 300 -50)
               :light-color (v! 20000 20000 30000)))
      (render-sky camera)
      ;;(draw-tex *convolved-env* 1f0 nil) ;; this has bugs
      ;;(draw-tex (dfg-lookup-sampler dfg) 1f0 nil)
      )))


;;----------------------------------------------------------------------
