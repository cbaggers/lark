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
                               (light-pos :vec3)
                               (roughness :float)
                               (metallic :float))
  ;;
  ;; Setup
  (let* ((f0 0.75)
         (f90 0.85)
         (world-pos (s~ (texture pos-sampler tc) :xyz))
         (normal (s~ (texture normal-sampler tc) :xyz))
         (albedo (v! 0.0 0.56 0.72))
         (view-dir (normalize (- world-pos)))
         (material (texture material-sampler tc))
         (metallic 1)
         (roughness (y material)))
    ;;
    (setf gl-frag-depth (x (texture depth tc)))
    ;;
    (let* (;; ibl
           (n·v (saturate (dot normal view-dir)))
           (ibl (calc-ibl dfg-lut specular-cube irradiance-map n·v normal
                          view-dir albedo metallic roughness
                          f0 f90)))
      ;;
      (tone-map-linear ibl 1s0))))

(def-g-> light-the-scene-pass ()
  (pass-through-vert g-pt)
  (light-the-scene-frag :vec2))

;;----------------------------------------------------------------------

(defvar *regen-light-probe* nil)

(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe env-map gbuffer) render-state

      (gl:clear :color-buffer-bit :depth-buffer-bit)

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
      (let ((thing (first (things *game-state*))))
        (loop :for y :below 4 :do
           (loop :for x :below 4 :do
              (setf (pos thing) (v3:+ (v! -150 130 -300)
                                      (v! (* x 100) (* y -90) 0)))
              (render-thing (update-thing thing) camera render-state
                            (/ (+ x (* 4 y)) 16)))))

      ;; draw & light
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
               :light-pos (v! 0 1000 -0)
               :metallic (/ (+ 1 (sin (/ (now) 1500))) 2s0))
        ;;(nineveh:draw-tex *convolved-env*)
        )

      (render-sky camera render-state)
      (swap))))


;;----------------------------------------------------------------------
