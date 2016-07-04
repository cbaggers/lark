(in-package #:assurance)

;;----------------------------------------------------------------------
;; Def

(deftclass (thing (:conc-name nil))
  model
  (in-space (make-space *world-space* (m4:identity)) :type vec-space)
  (pos (cepl:v! 0 0 -20) :type rtg-math.types:vec3)
  (rot (q:identity) :type rtg-math.types:quaternion)
  (base-sampler nil :type sampler)
  (normal-sampler nil :type sampler)
  (material-sampler nil :type sampler))


(defun load-thing (filepath base-tex-path normal-tex-path mat-tex-path)
  (let ((base (cepl.sdl2-image:load-image-to-texture base-tex-path))
	(norm (cepl.sdl2-image:load-image-to-texture normal-tex-path))
	(mat (cepl.sdl2-image:load-image-to-texture mat-tex-path)))
    (make-thing :model (yaksha:load-model filepath)
		:base-sampler (sample base)
		:normal-sampler (sample norm)
		:material-sampler (sample mat))))


(defun update-thing (thing)
  ;; populate space from transform
  (setf (get-transform (in-space thing) *world-space*)
	(m4:* (m4:translation (pos thing))
	      (q:to-mat4 (rot thing))))
  thing)


;; (defun render-thing (thing camera)
;;   (let ((gb (get-gbuffer)))
;;     (with-fbo-bound ((gbuffer-fbo gb))
;;       (clear)
;;       (using-camera camera
;; 	(loop :for mesh :in (yaksha:model-meshes (model thing)) :do
;; 	   (map-g #'drender-geom-pass (yaksha:mesh-stream mesh)
;; 		  :model-space (in-space thing)
;; 		  :diffuse-tex (first (yaksha:mesh-samplers mesh))
;; 		  :spec-tex nil))))
;;     (let ((light-pos
;; 	   (v! (* (cos (/ (now) 600)) 50)
;; 	       0
;; 	       (+ -20 (* (sin (/ (now) 600)) 50)))))
;;       (map-g #'drender-lighting-pass *quad-stream*
;; 	     :world-pos-sampler (gbuffer-pos-sampler gb)
;; 	     :world-norm-sampler (gbuffer-norm-sampler gb)
;; 	     :diff-and-spec-sampler (gbuffer-diff-spec-sampler gb)
;; 	     :camera-space (cepl.camera.base::base-camera-space camera)
;; 	     :light-pos light-pos
;; 	     :light-intensity 0.5))))
