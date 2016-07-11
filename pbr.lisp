(in-package :assurance)

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
  (mat-sampler (error "") :type sampler))

(deftclass (post-buff (:constructor %make-post-buff))
  (fbo (error "") :type fbo)
  (color-sampler (error "") :type sampler)
  (depth-sampler (error "") :type sampler))

(defmethod free ((gb gbuffer))
  ;;(free (gbuffer-fbo gbuff))
  (free (sampler-texture (gbuffer-pos-sampler gb)))
  (free (sampler-texture (gbuffer-norm-sampler gb)))
  (free (sampler-texture (gbuffer-base-sampler gb)))
  (free (sampler-texture (gbuffer-mat-sampler gb)))
  gb)

(defun make-gbuffer (&optional dimensions)
  ;; positions normals albedo specular
  (assert (listp dimensions))
  (let* ((dim (or dimensions (viewport-dimensions (current-viewport))))
	 (fbo (make-fbo `(0 :dimensions ,dim :element-type :rgb16f)
	   		`(1 :dimensions ,dim :element-type :rgb16f)
	   		`(2 :dimensions ,dim :element-type :rgb8)
	   		`(3 :dimensions ,dim :element-type :rgb8)
	   		`(:d :dimensions ,dim))))
    (%make-gbuffer
     :fbo fbo
     :pos-sampler (sample (attachment-tex fbo 0))
     :norm-sampler (sample (attachment-tex fbo 1))
     :base-sampler (sample (attachment-tex fbo 2))
     :mat-sampler (sample (attachment-tex fbo 3)))))

(defun resize-gbuffers (dimensions)
  (setf *gb* (make-gbuffer dimensions))
  (setf *post-buff* (make-post-buff dimensions))
  t)

(defun make-post-buff (&optional dimensions)
  (assert (listp dimensions))
  (let* ((dim (or dimensions (viewport-dimensions (current-viewport))))
	 (fbo (make-fbo `(0 :dimensions ,dim :element-type :rgb16f)
			`(:d :dimensions ,dim))))
    (%make-post-buff :fbo fbo
		     :color-sampler (sample (attachment-tex fbo 0))
		     :depth-sampler (sample (attachment-tex fbo :d)))))

(defvar *gb* nil)
(defvar *post-buff* nil)

(defun get-gbuffer ()
  (or *gb* (setf *gb* (make-gbuffer))))

(defun get-post-buff ()
  (or *post-buff* (setf *post-buff* (make-post-buff))))

;;----------------------------------------------------------------------

(defmacro bind-vec (vars vec &body body)
  (labels ((get-func (i var)
	     (let* ((var (if (listp var) var (list var)))
		    (f (second var))
		    (var (first var)))
	       `(,var ,(if f
			   (ecase f (:x 'x) (:y 'y) (:z 'z) (:w 'w))
			   (elt '(x y z w) i))))))
    (let ((v (gensym "vec")))
      `(let* ((,v ,vec)
	      ,@(loop :for p :in vars :for i :from 0
		   :for (c f) := (get-func i p)
		   :collect `(,c (,f ,v))))
	 ,@body))))

(defmacro-g bind-vec (vars vec &body body)
  (labels ((get-func (i var)
	     (let* ((var (if (listp var) var (list var)))
		    (f (second var))
		    (var (first var)))
	       `(,var ,(if f
			   (ecase f (:x 'x) (:y 'y) (:z 'z) (:w 'w))
			   (elt '(x y z w) i))))))
    (let ((v (gensym "vec")))
      `(let* ((,v ,vec)
	      ,@(loop :for p :in vars :for i :from 0
		   :for (c f) := (get-func i p)
		   :collect `(,c (,f ,v))))
	 ,@body))))

;;----------------------------------------------------------------------

;; phong (lambertian) diffuse term
(defun-g lambertian-diffuse ()
  (/ 1s0 +pi+))

;;----------------------------------------------------------------------
;; frostbite shading functions

;; [CHECK] This looks good
(defun-g fresnel-schlick ((f0 :vec3) (f90 :float) (u :float))
  (+ f0
     (* (- (v3! f90) f0)
	(pow (- 1s0 u) 5s0))))

;; [CHECK] This looks good, linear-roughness is interesting though, is our
;;         roughness ok?
(defun-g disney-diffuse ((n·v :float) (n·l :float) (l·h :float)
			 (linear-roughness :float))
  ;; with renormalization of it's energy
  (let* ((energy-bias (mix 0.0 0.5 linear-roughness))
	 (energy-factor (mix 1.0 (/ 1.0 1.51) linear-roughness))
	 (l·h² (* l·h l·h))
	 (fd90 (+ energy-bias (* 2.0 l·h² linear-roughness)))
	 (f0 (v! 1 1 1))
	 (light-scatter (x (fresnel-schlick f0 fd90 n·l)))
	 (view-scatter (x (fresnel-schlick f0 fd90 n·v))))
    (* light-scatter view-scatter energy-factor)))


;; [CHECK] This looks good
(defun-g ggx-distribution ((n·h :float) (m :float))
  (let* ((m² (* m m))
	 (f (+ 1.0 (* (- (* n·h m²)
			 n·h)
		      n·h)))
	 (f² (* f f)))
    ;; we don't divide by +pi+ here as we will do that in the main brdf function
    (/ m² f²)))


;; Original formulation of G_SmithGGX Correlated
;;
;; lambda_v = ( -1 + sqrt ( alphaG2 * (1 - NdotL2 ) / NdotL2 + 1) ) * 0.5 f ;
;; lambda_l = ( -1 + sqrt ( alphaG2 * (1 - NdotV2 ) / NdotV2 + 1) ) * 0.5 f ;
;; G_SmithGGXCorrelated = 1 / (1 + lambda_v + lambda_l);
;; V_SmithGGXCorrelated = G_SmithGGXCorrelated / (4.0 f * N·V * N·L ) ;
;;
;; below is the optimized version
;; [CHECK] This looks good
(defun-g ggx-geom-smith-correlated ((n·v :float) (n·l :float) (α :float))
  (let* ((α² (* α α))
	 (ggx-v-λ (* n·l (sqrt (+ (* (+ (* (- n·v) α²)
					n·v)
				     n·v)
				  α²))))
	 (ggx-l-λ (* n·v (sqrt (+ (* (+ (* (- n·l) α²)
					n·l)
				     n·l)
				  α²)))))
    (/ 0.5 (+ ggx-v-λ ggx-l-λ))))


;; [CHECK] This looks good
(defun-g specular-brdf ((n·v :float) (l·h :float) (n·h :float) (n·l :float)
			(h :vec3) (f0 :vec3) (f90 :float) (roughness :float))
  (let ((f (fresnel-schlick f0 f90 l·h))
	(g (ggx-geom-smith-correlated n·v n·l roughness))
	(d (ggx-distribution n·h roughness)))
    (/ (* d g f) +pi+)))


;;----------------------------------------------------------------------
;; frostbite punctual light

(defun-g smooth-distance-attenuation
    ((distance² :float) (inverse-square-attenuation-radius :float))
  (let* ((factor (* distance² inverse-square-attenuation-radius))
	 (smooth-factor (saturate (- 1.0 (* factor factor)))))
    (* smooth-factor smooth-factor)))

(defun-g get-distance-attenuation ((unormalized-light-vec :vec3)
				   (inverse-square-attenuation-radius :float))
  (let* ((distance² (dot unormalized-light-vec unormalized-light-vec))
	 (attenuation (/ 1.0 (max distance² (* 0.01 0.01)))))
    (* attenuation (smooth-distance-attenuation
		    distance²
		    inverse-square-attenuation-radius))))


  ;; On the CPU it would be
  ;; float lightAngleScale = 1.0f / max (0.001f, (cosInner - cosOuter));
  ;; float lightAngleOffset = -cosOuter * angleScale;
  ;;
(defun-g get-angle-attenuation ((normalized-light-vec :vec3) (light-dir :vec3)
				(light-angle-scale :float)
				(light-angle-offset :float))
  (let* ((cd (dot light-dir normalized-light-vec))
	 (attenuation (saturate (+ (* cd light-angle-scale)
				   light-angle-offset))))
    ;; squaring smooths the transition
    (* attenuation attenuation)))

(defun-g punctual-light-luminance
    ((wpos :vec3) (normal :vec3) (view-dir :vec3)
     (base-color :vec3) (roughness :float) (metallic :float) (ao :float)
     (light-pos :vec3) (light-color :vec3) (light-inv-sqr-att-radius :float))
  ;;
  (let* ((unormalized-light-vec (- light-pos wpos))
	 (normalized-light-vec (normalize unormalized-light-vec))
	 ;;
	 (half-vec (normalize (+ normalized-light-vec view-dir)))
	 (n·v (+ (abs (dot normal view-dir)) 0.00001))
	 (l·h (saturate (dot normalized-light-vec half-vec)))
	 (n·h (saturate (dot normal half-vec)))
	 (n·l (saturate (dot normal normalized-light-vec)))
	 ;;
	 (linear-roughness roughness) ;; ← These two are super wrong
	 (roughness roughness)        ;; ← see notes for the bit where I'm lost
	 ;;
	 (f0 (mix (v! 0.04 0.04 0.04) base-color (- 1 metallic))) ;; ← probably
	 (f90 1.0)                                                ;; ← wrong
	 ;;
	 (fd (/ (disney-diffuse n·v n·l l·h linear-roughness) +pi+))
	 (fr (specular-brdf n·v l·h n·h n·l half-vec f0 f90 roughness))
	 ;;
	 (diffuse (* base-color fd)) ;; definitely wrong
	 ;;
	 (ks (x (saturate (fresnel-schlick f0 f90 l·h)))) ;; ← again not sure if
	 (kd (- 1 ks))                                    ;; ← this makes sense
	 (final (+ (* kd diffuse) (* ks fr)))
	 ;;
	 (attenuation (get-distance-attenuation
		       unormalized-light-vec light-inv-sqr-att-radius)))
    (* final
       (saturate (dot normal normalized-light-vec))
       light-color
       attenuation
       ;;ao
       1)
    ;; (let ((p roughness))
    ;;   (v! p p p))
    ;; base-color
    ))

;;----------------------------------------------------------------------
;; Approach

;; - diffuse brdf - lambertian
;; - specular brdf
;;   - D - ggx/trowbridge-reitz
;;   - G - Schlick with unreal modification for analytics lights
;;   - F - schlick fresnel



;;----------------------------------------------------------------------

(defun-g pack-gbuffer-vert ((vert yaksha:vertex) &uniform (model-space vec-space))
  (let* ((m->w (m4:to-mat3 (get-transform model-space *world-space*)))
	 (normal (* m->w (- (* (yaksha:normal vert) 2) (v! 1 1 1))))
	 (tangent (* m->w (yaksha:tangent vert)))
	 (bitangent (cross normal tangent))
	 (btn-mat (m! tangent bitangent normal)))
    (values (in *clip-space*
	      (in model-space (sv! (pos vert) 1.0)))
	    (in *world-space*
	      (in model-space (sv! (pos vert) 1.0)))
	    btn-mat
	    (yaksha:uv vert))))

(defun-g pack-gbuffer-frag ((world-pos :vec4) (btn-mat :mat3) (uv :vec2)
			    &uniform (base-tex :sampler-2d)
			    (norm-tex :sampler-2d)
			    (mat-tex :sampler-2d))
  (let ((mat (texture mat-tex uv)))
    (values world-pos
	    (* btn-mat (s~ (texture norm-tex uv) :xyz))
	    (s~ (texture base-tex uv) :xyz)
	    (v! (x mat) (y mat) 1))))


(def-g-> pack-gbuffer-pass ()
  #'pack-gbuffer-vert #'pack-gbuffer-frag)

;;----------------------------------------------------------------------

(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1)
	  (tex vert)))

;;----------------------------------------------------------------------

(defun-g my-pbr-analytic-light-frag
    ((tc :vec2) &uniform (wview-dir :vec3) (light-origin :vec3)
     (light-radius :float) (light-radiance :vec3) (pos-sampler :sampler-2d)
     (normal-sampler :sampler-2d) (base-sampler :sampler-2d)
     (mat-sampler :sampler-2d))
  (let* ((wpos (s~ (texture pos-sampler tc) :xyz))
	 (wnormal (normalize (s~ (texture normal-sampler tc) :xyz)))
	 (base-color (s~ (texture base-sampler tc) :xyz))
	 (material (texture mat-sampler tc))
	 (metallic (x material))
	 (roughness (y material))
	 (ao 1.0)
	 (light-inv-sqr-att-radius (/ 1 (pow light-radius 2))))
    (punctual-light-luminance
     wpos wnormal wview-dir
     base-color roughness metallic ao
     light-origin light-radiance light-inv-sqr-att-radius)))

(def-g-> pbr-pass ()
  #'pass-through-vert my-pbr-analytic-light-frag)

;;----------------------------------------------------------------------

(defun-g my-pbr-post-prog-frag
    ((tex-coord :vec2) &uniform (linear-final :sampler-2d))
  (tone-map-reinhard
   (s~ (texture linear-final tex-coord) :xyz)
   16s0))

(def-g-> pbr-post-pass ()
  #'pass-through-vert my-pbr-post-prog-frag)

;;----------------------------------------------------------------------

(defun render-thing (thing camera)
  (let ((time (/ (now) 3200))
	(gb (get-gbuffer))
  	(pb (get-post-buff)))
    (setf (rot thing) (q:from-mat3 (m3:rotation-from-euler
				    (v! (* 2 (cos time)) (sin time) (sin time)))))
    (using-camera camera
      (with-fbo-bound ((gbuffer-fbo gb))
      	(clear)
	(loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	   (map-g #'pack-gbuffer-pass (yaksha:mesh-stream mesh)
		  :model-space (in-space thing)
		  :base-tex (base-sampler thing)
		  :norm-tex (normal-sampler thing)
		  :mat-tex (material-sampler thing))))
      (let* ((time (/ (now) 600))
	     (light-pos
	      (v! -4 30 0)
	       ;; (v:+ (v! 0 0 -120)
	       ;; 	   (v3:*s (v! (cos time) 0 (* (sin time)))
	       ;; 		  100s0))
	       ))
	(with-fbo-bound ((post-buff-fbo pb))
	  (clear)
	  (map-g #'pbr-pass *quad-stream*
		 :wview-dir (v! 0 0 -1)
		 :light-origin light-pos
		 :light-radius 500s0
		 :light-radiance (v! 600.3 600.3 600.3)
		 :pos-sampler (gbuffer-pos-sampler gb)
		 :normal-sampler (gbuffer-norm-sampler gb)
		 :base-sampler (gbuffer-base-sampler gb)
		 :mat-sampler (gbuffer-mat-sampler gb))))
      (map-g #'pbr-post-pass *quad-stream*
      	     :linear-final (post-buff-color-sampler pb)))))

;;----------------------------------------------------------------------

(defun-g debug-draw-sampler-frag ((tc :vec2) &uniform (s :sampler-2d))
  (texture s tc 0))

(def-g-> debug-draw-sampler ()
  #'pass-through-vert #'debug-draw-sampler-frag)

(defun draw-sampler (sampler)
  (cls)
  (map-g #'debug-draw-sampler *quad-stream* :s sampler)
  (swap))

(defun-g green-frag ((tc :vec2) &uniform (s :sampler-2d))
  (v! 0 1 0 1))

(def-g-> green-pass ()
  #'pass-through-vert #'green-frag)

(defun green ()
  (map-g #'green-pass *quad-stream*))

;; (setf (pos (first (things *game-state*))) (v! 0 0 -120))

;;----------------------------------------------------------------------

(defun down-to-nearest (x n)
  (* (floor x n) n))

(defun reshape (new-resolution)
  (let ((new-resolution (v! (down-to-nearest (v:x new-resolution) 8)
			    (down-to-nearest (v:y new-resolution) 8)))
	(new-dimensions (list (v:x new-resolution) (v:y new-resolution))))
    ;;(format t "~%New Resolution: ~a~%" new-resolution)
    (setf (viewport-resolution (camera-viewport *camera*)) new-resolution)
    (resize-gbuffers new-dimensions)))
