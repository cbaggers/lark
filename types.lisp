(in-package :lark)

(defvar dfg-sampler nil)
(defvar dfg-fbo nil)
(defvar light-probe-sampler nil)
(defvar light-probe-fbo nil)
(defvar light-probe-specular-sampler nil)
(defvar light-probe-specular-fbo nil)

;;----------------------------------------------------------------------
;; GBuffer
;;
;; rgb16f pos
;; rgb16f normal
;; rgb8 base-color
;; rgb8 metallic, roughness, AO

(deftclass (gbuffer (:constructor %make-gbuffer))
  (fbo (error "") :type fbo)
  (pos-sampler (error "") :type sampler)
  (norm-sampler (error "") :type sampler)
  (base-sampler (error "") :type sampler)
  (mat-sampler (error "") :type sampler)
  (depth-sampler (error "") :type sampler))

;;----------------------------------------------------------------------

(deftclass (post-buff (:constructor %make-post-buff))
  (fbo (error "") :type fbo)
  (color-sampler (error "") :type sampler)
  (depth-sampler (error "") :type sampler))
