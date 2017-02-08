(in-package #:lark)

;;----------------------------------------------------------------------
;; Def

(deftclass (thing (:conc-name nil))
  model
  (model-space (make-space *world-space* (m4:identity)) :type vec-space)
  (offset (v! 0 40 0))
  (pos (v! 0 0 -20) :type rtg-math.types:vec3)
  (rot (q:identity) :type rtg-math.types:quaternion)
  (base-sampler nil :type sampler)
  (normal-sampler nil :type sampler)
  (metallic-sampler nil :type sampler)
  (roughness-sampler nil :type sampler))


(defun load-thing (filepath base-tex-path normal-tex-path met-tex-path
                   rough-tex-path
                   &key (pos (v! 0 0 -120)))
  (let ((base (cepl.sdl2-image:load-image-to-texture
               base-tex-path :srgb8-alpha8 t t))

        (norm (cepl.sdl2-image:load-image-to-texture
               normal-tex-path :rgba8 t t))

        (met (cepl.sdl2-image:load-image-to-texture
              met-tex-path :rgba8 t t))

        (rough (cepl.sdl2-image:load-image-to-texture
                rough-tex-path :rgba8 t t)))
    (make-thing :pos pos
                :model (yaksha:load-model filepath)
                :base-sampler
                (sample base :minify-filter :linear-mipmap-linear)
                :normal-sampler
                (sample norm :minify-filter :linear-mipmap-linear)
                :metallic-sampler
                (sample met :minify-filter :linear-mipmap-linear)
                :roughness-sampler
                (sample rough :minify-filter :linear-mipmap-linear))))

(defun make-sphere-thing (base-tex-path normal-tex-path met-tex-path
                          rough-tex-path
                          &key (pos (v! 0 0 -120)))
  (let ((base (dirt:load-image-to-texture
               base-tex-path :srgb8-alpha8 t t))

        (norm (dirt:load-image-to-texture
               normal-tex-path :rgba8 t t))

        (met (dirt:load-image-to-texture
              met-tex-path :rgba8 t t))

        (rough (dirt:load-image-to-texture
                rough-tex-path :rgba8 t t)))
    (make-thing :pos pos
                :model (make-sphere-model)
                :base-sampler
                (sample base :minify-filter :linear-mipmap-linear)
                :normal-sampler
                (sample norm :minify-filter :linear-mipmap-linear)
                :metallic-sampler
                (sample met :minify-filter :linear-mipmap-linear)
                :roughness-sampler
                (sample rough :minify-filter :linear-mipmap-linear))))


(defun update-thing (thing)
  ;; populate space from transform
  (setf (get-transform (model-space thing) *world-space*)
        (m4:* (m4:translation (pos thing))
              (q:to-mat4 (q:normalize (rot thing)))
              ;; (m4:* (q:to-mat4 (q:normalize (rot thing)))
              ;;         (m4:translation (offset thing)))
              ))
  thing)
