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
;; Shader pipeline

(deftclass gbuffer
  (front (error "") :type fbo)
  (back (error "") :type fbo))

(defun-g first-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (in *clip-space*
    (values (in model-space (sv! (pos vert) 1.0))
	    (pos vert)
	    (yaksha:normal vert)
	    (yaksha:uv vert))))

(defun-g first-frag ((norm :vec3) (pos :vec3) (uv :vec2) &uniform (tex :sampler-2d)
		     (camera-space vec-space) (model-space vec-space)
		     (light-pos :vec3) (light-intensity :float))
  (let ((sun-cos-angle-of-incidence
	 (varjo-lang:clamp
	  (in camera-space
	    (varjo-lang:dot (v:normalize (in model-space (sv! norm 0)))
			    (in *world-space* (sv! *dir-to-sun*))))
	  0 1))
	(diffuse-color (varjo-lang:texture tex uv))
	(light-cos-angle-of-incidence
	 (varjo-lang:clamp
	  (in model-space
	    (varjo-lang:dot (v:normalize (sv! norm 0))
			    (sv! (v:normalize
				  (- (in *world-space* (sv! light-pos 0))
				     (v! pos 0))))))
	  0 1)))
    (+ (* diffuse-color *ambient-intensity*)
       (* light-intensity diffuse-color light-cos-angle-of-incidence)
       ;;(* *sun-intensity* diffuse-color sun-cos-angle-of-incidence)
       )))

(def-g-> first-render ()
  #'first-vert #'first-frag)

;;----------------------------------------------------------------------
;;

(defun update-thing (thing)
  ;; populate space from transform
  (setf (get-transform (in-space thing) *world-space*)
	(m4:* (m4:translation (pos thing))
	      (q:to-mat4 (rot thing))))
  thing)

(defun render-thing (thing camera)
  (let ((light-pos
	 (v! (* (cos (/ (now) 600)) 50)
	     0
	     (+ -20 (* (sin (/ (now) 600)) 50)))))
    (using-camera camera
      (loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	 (map-g #'first-render (yaksha:mesh-stream mesh)
		:model-space (in-space thing)
		:camera-space (cepl.camera.base::base-camera-space camera)
		:tex (first (yaksha:mesh-samplers mesh))
		:light-pos light-pos
		:light-intensity 0.5)))))

;;----------------------------------------------------------------------
