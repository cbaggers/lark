(in-package :lark)

(defvar *started* nil)
(defvar *game-state* nil)
(defvar *catwalk* nil)
(defvar *render-state* nil)

;;----------------------------------------------------------------------

(deftclass (game-state (:conc-name nil))
  things
  (render-state (make-render-state)))

;;----------------------------------------------------------------------
;; GBuffer
;;
;; rgb32f pos
;; rgb16f normal
;; rgb8 base-color
;; rgb8 metallic, roughness, AO

(deftclass (gbuffer (:constructor %make-gbuffer) (:conc-name nil))
  (fbo (error "") :type fbo)
  (pos-sampler (error "") :type sampler)
  (norm-sampler (error "") :type sampler)
  (base-sampler (error "") :type sampler)
  (mat-sampler (error "") :type sampler)
  (depth-sampler (error "") :type sampler))

(defun make-gbuffer (&optional dimensions)
  ;; positions normals albedo specular
  (assert (listp dimensions))
  (let* ((dim (or dimensions (viewport-dimensions (current-viewport))))
	 (fbo (make-fbo `(0 :dimensions ,dim :element-type :rgb32f)
	   		`(1 :dimensions ,dim :element-type :rgb16f)
	   		`(2 :dimensions ,dim :element-type :rgb8)
	   		`(3 :dimensions ,dim :element-type :rgb8)
	   		`(:d :dimensions ,dim))))
    (%make-gbuffer
     :fbo fbo
     :pos-sampler (sample (attachment-tex fbo 0))
     :norm-sampler (sample (attachment-tex fbo 1))
     :base-sampler (sample (attachment-tex fbo 2))
     :mat-sampler (sample (attachment-tex fbo 3))
     :depth-sampler (sample (attachment-tex fbo :d)))))

(defmethod free ((gb gbuffer))
  ;;(free (fbo gbuff))
  (free (sampler-texture (pos-sampler gb)))
  (free (sampler-texture (norm-sampler gb)))
  (free (sampler-texture (base-sampler gb)))
  (free (sampler-texture (mat-sampler gb)))
  gb)

;;----------------------------------------------------------------------

(defconstant +ibl-mipmap-count+ 5)

(deftclass (light-probe (:constructor %make-light-probe) (:conc-name nil))
  diffuse-cube
  diffuse-fbo
  diffuse-sampler
  specular-cube
  specular-fbos
  specular-sampler)

(defun make-light-probe (&optional (dimensions '(128 128)))
  (assert (and (>= (first dimensions) 128)
               (>= (second dimensions) 128)))
  (let ((diff-cube (make-texture nil :dimensions dimensions :cubes t
                                 :element-type :rgb16f))
        (spec-cube (make-texture nil :dimensions dimensions :cubes t
                                 :element-type :rgb16f
                                 :mipmap +ibl-mipmap-count+)))
    (%make-light-probe
     :diffuse-cube diff-cube
     :diffuse-fbo (make-fbo diff-cube :d)
     :diffuse-sampler (sample diff-cube)

     :specular-cube spec-cube
     :specular-fbos (make-fbos-for-each-mipmap-of-cube-texture spec-cube)
     :specular-sampler (sample spec-cube))))

;;----------------------------------------------------------------------

(deftclass (dfg-lookup (:constructor %make-dfg-lookup) (:conc-name nil))
  fbo
  tex
  sampler)

(defun make-dfg-lookup (&optional (dimensions '(128 128)))
  (let ((dfg-tex (make-texture nil :dimensions dimensions
			       :element-type :rgb16f)))
    ;; apparently this could be r16g16f (rg16f?) ↑↑
    (%make-dfg-lookup
     :fbo (make-fbo `(0 ,dfg-tex) `(:d :dimensions ,dimensions))
     :tex dfg-tex
     :sampler (sample dfg-tex))))

;;----------------------------------------------------------------------

(deftclass (render-state (:conc-name nil))
  (dfg (make-dfg-lookup))
  (light-probe (make-light-probe))
  (gbuffer (make-gbuffer))
  (env-map (sample (load-hdr-cross-texture
                    (path "media/galileo_cross.hdr" t)))))
