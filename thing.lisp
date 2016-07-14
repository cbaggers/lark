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
  (let ((base (cepl.sdl2-image:load-image-to-texture
	       base-tex-path :srgb8-alpha8 t t))
	(norm (cepl.sdl2-image:load-image-to-texture
	       normal-tex-path :srgb8-alpha8 t t))
	(mat (cepl.sdl2-image:load-image-to-texture
	      mat-tex-path :srgb8-alpha8 t t)))
    (make-thing :model (yaksha:load-model filepath)
		:base-sampler (sample base)
		:normal-sampler (sample norm)
		:material-sampler (sample mat))))


(defun update-thing (thing)
  ;; populate space from transform
  (setf (get-transform (in-space thing) *world-space*)
	(m4:* (m4:translation (pos thing))
	      (q:to-mat4 (q:normalize (rot thing)))))
  thing)
