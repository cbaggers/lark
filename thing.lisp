(in-package #:assurance)

;;----------------------------------------------------------------------
;; Def

(deftclass (thing (:conc-name nil))
  model
  (in-space (make-space *world-space* (m4:identity)) :type vec-space)
  (pos (cepl:v! 0 0 -20) :type rtg-math.types:vec3)
  (rot (q:identity) :type rtg-math.types:quaternion))

(defun load-thing (filepath)
  (make-thing :model (yaksha:load-model filepath)))

;;----------------------------------------------------------------------
;; GBuffer

(deftclass (gbuffer (:constructor %make-gbuffer))
  (fbo (error "") :type fbo)
  (pos-sampler (error "") :type sampler)
  (norm-sampler (error "") :type sampler)
  (diff-spec-sampler (error "") :type sampler))

(defun make-gbuffer ()
  ;; positions normals albedo specular
  (let* ((dim (viewport-dimensions (current-viewport)))
	 (fbo (make-fbo `(0 :dimensions ,dim :element-type :rgb16f)
			`(1 :dimensions ,dim :element-type :rgb16f)
			`(2 :dimensions ,dim :element-type :rgba8)
			:d)))
    (%make-gbuffer
     :fbo fbo
     :pos-sampler (sample (attachment-tex fbo 0))
     :norm-sampler (sample (attachment-tex fbo 1))
     :diff-spec-sampler (sample (attachment-tex fbo 2)))))

(defvar *gb* nil)

(defun get-gbuffer ()
  (or *gb* (setf *gb* (make-gbuffer))))

;;----------------------------------------------------------------------
;; Shader pipeline

(defun-g geom-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (values (in *clip-space* (in model-space (sv! (pos vert) 1.0)))
	  (in *world-space* (in model-space (sv! (pos vert) 1.0)))
	  (in *world-space* (in model-space (sv! (yaksha:normal vert) 0.0))))
  (yaksha:uv vert))

(defun-g geom-frag ((world-pos :vec4) (world-norm :vec4) (uv :vec2)
		     &uniform (diffuse-tex :sampler-2d) (spec-tex :sampler-2d))
  (let ((diffuse-color (s~ (texture diffuse-tex uv) :xyz))
	(specular 0.1;; (s~ (texture spec-tex uv) :x)
	  ))
    (values world-pos
	    world-norm
	    (v! diffuse-color specular))))

(def-g-> drender-geom-pass ()
  #'geom-vert #'geom-frag)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1)
	  (tex vert)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g defered-lighting-frag ((tc :vec2) &uniform
				(world-pos-sampler :sampler-2d)
				(world-norm-sampler :sampler-2d)
				(diff-and-spec-sampler :sampler-2d)
				(camera-space vec-space)
				(light-pos :vec3)
				(light-intensity :float))
  (let* ((diff-n-spec (texture diff-and-spec-sampler tc))
	 (diffuse (s~ diff-n-spec :xyz))
	 (specular (s~ diff-n-spec :w))
	 (world-pos (texture world-pos-sampler tc))
	 (world-norm (v:normalize (texture world-norm-sampler tc)))
	 (light-cos-angle-of-incidence
	  (varjo-lang:clamp
	   (varjo-lang:dot world-norm
			   (v:normalize
			    (- (v! light-pos 0)
			       (v! (s~ world-pos :xyz) 0))))
	   0 1)))

    (+ (* diffuse *ambient-intensity*)
       (* light-intensity diffuse light-cos-angle-of-incidence))))

(def-g-> drender-lighting-pass ()
  #'pass-through-vert #'defered-lighting-frag)

;;----------------------------------------------------------------------
;;

(defun update-thing (thing)
  ;; populate space from transform
  (setf (get-transform (in-space thing) *world-space*)
	(m4:* (m4:translation (pos thing))
	      (q:to-mat4 (rot thing))))
  thing)

(defun render-thing (thing camera)
  (let ((gb (get-gbuffer)))
    (with-fbo-bound ((gbuffer-fbo gb))
      (clear)
      (using-camera camera
	(loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	   (map-g #'drender-geom-pass (yaksha:mesh-stream mesh)
		  :model-space (in-space thing)
		  :diffuse-tex (first (yaksha:mesh-samplers mesh))
		  :spec-tex nil))))
    (let ((light-pos
	   (v! (* (cos (/ (now) 600)) 50)
	       0
	       (+ -20 (* (sin (/ (now) 600)) 50)))))
      (map-g #'drender-lighting-pass *quad-stream*
	     :world-pos-sampler (gbuffer-pos-sampler gb)
	     :world-norm-sampler (gbuffer-norm-sampler gb)
	     :diff-and-spec-sampler (gbuffer-diff-spec-sampler gb)
	     :camera-space (cepl.camera.base::base-camera-space camera)
	     :light-pos light-pos
	     :light-intensity 0.5))))

;;----------------------------------------------------------------------
