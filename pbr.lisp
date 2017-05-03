(in-package :lark)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------
;; render with ibl

(defun-g light-the-scene-frag ((tc :vec2) &uniform
                               (albedo-sampler :sampler-2d)
                               (pos-sampler :sampler-2d)
                               (normal-sampler :sampler-2d)
                               (material-sampler :sampler-2d)
                               (specular-cube :sampler-cube)
                               (irradiance-map :sampler-2d)
                               (dfg-lut :sampler-2d)
                               (depth :sampler-2d)
                               (light-pos :vec3))
  ;;
  ;; Setup
  (let* (;; both
         (world-pos (s~ (texture pos-sampler tc) :xyz))
         (normal (s~ (texture normal-sampler tc) :xyz))
         (albedo (s~ (texture albedo-sampler tc) :xyz))
         (view-dir (normalize (- world-pos)))
         (material (texture material-sampler tc))
         (metallic (x material))
         (roughness (y material))
         ;; punctual light
         (light-dir (normalize (- light-pos world-pos))))
    ;;
    (setf gl-frag-depth (x (texture depth tc)))
    ;;
    (let* (;;
           (n·v (saturate (dot normal view-dir)))
           ;; punctual
           (half-vec (normalize (+ view-dir light-dir)))
           (l·h (saturate (dot light-dir half-vec)))
           (n·h (saturate (dot normal half-vec)))
           (n·l (saturate (dot normal light-dir)))
           (linear-roughness (* roughness roughness)) ;; perceptualy linear roughness (α)
           (plight (punctual-light albedo n·v half-vec
                                   l·h n·h n·l
                                   linear-roughness
                                   roughness
                                   metallic))
           ;; ibl
           (ibl (calc-ibl dfg-lut specular-cube irradiance-map n·v normal
                          view-dir albedo metallic roughness))
           ;; combine
           (final (+ plight ibl)))
      ;;
      (tone-map-uncharted2 final 1s0 1.0))))

(def-g-> light-the-scene-pass ()
  (pass-through-vert g-pt)
  (light-the-scene-frag :vec2))

;;----------------------------------------------------------------------

(defvar *regen-light-probe* nil)

(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe env-map gbuffer) render-state
      (when *regen-light-probe*
        (setf *regen-light-probe* nil)

        ;; regen the dfg-lut (only ever needs to be done once.. but meh)
        (clear-fbo (fbo dfg))
        (map-g-into (fbo dfg) #'compute-dfg-lut-pass *quad-stream*)

        ;; specular ggx
        (loop :for fbo :in (specular-fbos light-probe) :for i :from 0 :do
           (clear-fbo fbo)
           (let* ((step (/ 1 +ibl-mipmap-count+))
                  (roughness (* i step)))
             (map-g-into fbo #'iblggx-convolve-pass *quad-stream*
                         :env-map *catwalk*
                         :roughness roughness))))

      ;;
      (clear-fbo (fbo gbuffer))

      ;; populate the gbuffer
      (map nil λ(render-thing (update-thing _) camera render-state)
           (things *game-state*))

      ;;draw & light
      (using-camera camera
        (map-g #'light-the-scene-pass *quad-stream*
               :pos-sampler (pos-sampler gbuffer)
               :albedo-sampler (base-sampler gbuffer)
               :normal-sampler (norm-sampler gbuffer)
               :material-sampler (mat-sampler gbuffer)
               :specular-cube (specular-sampler light-probe)
               :irradiance-map *convolved-env*
               :dfg-lut (sampler dfg)
               :depth (depth-sampler gbuffer)
               :light-pos (v! 0 1000 -0))
        ;;(draw-tex (mat-sampler gbuffer))
        )
      (render-sky camera))))


;;----------------------------------------------------------------------
