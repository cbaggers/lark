(in-package :assurance)

(defvar qoob nil)
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
     :mat-sampler (sample (attachment-tex fbo 3))
     :depth-sampler (sample (attachment-tex fbo :d)))))

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

(defun-g fresnel-schlick ((f0 :vec3) (f90 :float) (u :float))
  (+ f0
     (* (- (v3! f90) f0)
	(pow (- 1s0 u) 5s0))))

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


;; Original formulation of G_SmithGGX Correlated
;;
;; lambda_v = ( -1 + sqrt ( alphaG2 * (1 - NdotL2 ) / NdotL2 + 1) ) * 0.5 f ;
;; lambda_l = ( -1 + sqrt ( alphaG2 * (1 - NdotV2 ) / NdotV2 + 1) ) * 0.5 f ;
;; G_SmithGGXCorrelated = 1 / (1 + lambda_v + lambda_l);
;; V_SmithGGXCorrelated = G_SmithGGXCorrelated / (4.0 f * N·V * N·L ) ;
;;
;; below is the optimized version
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

(defun-g punctual-light-luminance
    ((wpos :vec3) (normal :vec3) (view-dir :vec3) (n·v :float)
     (base-color :vec3) (roughness :float) (metallic :float) (ao :float)
     (light-pos :vec3) (light-color :vec3) (light-inv-sqr-att-radius :float))
  ;;
  (let* ((unormalized-light-vec (- light-pos wpos))
	 (normalized-light-vec (normalize unormalized-light-vec))
	 ;;
	 (half-vec (normalize (+ normalized-light-vec view-dir)))
	 (l·h (saturate (dot normalized-light-vec half-vec)))
	 (n·h (saturate (dot normal half-vec)))
	 (n·l (saturate (dot normal normalized-light-vec)))
	 ;;
	 (albedo (mix base-color (v3! 0) metallic))
	 ;;
	 (linear-roughness roughness)
	 (roughness (* linear-roughness linear-roughness))
	 ;;
	 ;; frostbite parameterizes this calling it specular
	 ;; which really confused me. Unreal just use 0.04.
	 ;;           ↓
	 (f0 (mix (v3! 0.04) base-color metallic))
	 (f90 (saturate (* 50s0 (dot f0 (v3! 0.33)))))
	 ;;
	 (fd (/ (disney-diffuse n·v n·l l·h linear-roughness) +pi+))
	 (fr (specular-brdf n·v l·h n·h n·l half-vec f0 f90 roughness))
	 ;;
	 (final (+ (* albedo fd) fr))
	 ;;
	 (attenuation (get-distance-attenuation
		       unormalized-light-vec light-inv-sqr-att-radius)))
    (* final
       n·l
       light-color
       attenuation
       )))

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
  (let ((mat (texture mat-tex uv))
	(wnormal (* btn-mat (s~ (texture norm-tex uv) :xyz))))
    (values world-pos
	    wnormal
	    (s~ (texture base-tex uv) :xyz)
	    (v! (x mat) (y mat) 1))))


(def-g-> pack-gbuffer-pass ()
  #'pack-gbuffer-vert #'pack-gbuffer-frag)

;;----------------------------------------------------------------------

(defun-g linear-roughness-to-mip-level ((linear-roughness :float)
					(mip-count :int))
  (* (sqrt linear-roughness) mip-count))

(defun-g get-specular-dominant-dir ((n :vec3) (r :vec3) (roughness :float))
  (let* ((smoothness (saturate (- 1 roughness)))
	 (lerp-factor (* smoothness (+ (sqrt smoothness) roughness))))
    (mix n r lerp-factor)))

(defun-g evaluate-ibl-specular ((n :vec3) (r :vec3) (n·v :float)
				(linear-roughness :float) (roughness :float)
				(f0 :vec3) (f90 :float) (cube :sampler-cube)
				(ld-mip-max-level :int) (dfg :sampler-2d))
  (let* ((dominant-r (get-specular-dominant-dir n r roughness))
	 (dfg-tex-size 128)
	 ;; Rebuild the function
	 ;; L . D. ( f0.Gv.(1-Fc) + Gv.Fc ) . cosTheta / (4 . NdotL . NdotV)
	 (n.v (max n·v (/ 0.5 dfg-tex-size)))
	 (mip-level (linear-roughness-to-mip-level linear-roughness
						   ld-mip-max-level))
	 (pre-ld (texture-lod cube dominant-r mip-level))
	 (pre-dfg (s~ (texture dfg (v! n·v roughness)) :xy)))
    ;; usual follow is (v! .. (w pre-ld)) but we arent using alpha
    (* (+ (* f0 (x pre-dfg))
	  (* (v3! f90) (y pre-dfg)))
       (s~ pre-ld :xyz))))

(defun-g get-diffuse-dominant-dir ((n :vec3) (v :vec3) (n·v :float)
				   (roughness :float))
  (let* ((a (- (* 1.02341 roughness) 1.51174))
	 (b (+ (* -0.511705 roughness) 0.755868))
	 (lerp-factor (saturate (* (+ (* n·v a) b) roughness))))
    (mix n v lerp-factor)))


(defun-g evaluate-ibl-diffuse ((n :vec3) (v :vec3) (n·v :float)
			       (roughness :float) (cube :sampler-cube)
			       (dfg :sampler-2d))
  (let* ((dominant-n (get-diffuse-dominant-dir n v n·v roughness))
	 (diffuse-lighting (texture cube dominant-n))
	 (diff-f (z (texture dfg (v! n·v roughness)))))
    (v! (* (s~ diffuse-lighting :xyz) diff-f) (w diffuse-lighting))))



(defun-g evaluate-ibl ((wnormal :vec3) (n·v :float) (roughness :float)
		       (cube :sampler-cube) (specular-cube :sampler-cube)
		       (dfg :sampler-2d))
  (+ (s~ (evaluate-ibl-diffuse wnormal (v! 0 0 -1) n·v roughness cube dfg) :xyz)
     (let ((linear-roughness 0.1)
	   (r wnormal)
	   (f0 (v3! 0.04))
	   (f90 0.8s0))
       (evaluate-ibl-specular wnormal r n·v linear-roughness roughness
			      f0 f90 specular-cube 4 dfg))))

;;----------------------------------------------------------------------

(defun-g my-pbr-analytic-light-frag
    ((tc :vec2) &uniform (wview-dir :vec3) (light-origin :vec3)
     (light-radius :float) (light-radiance :vec3) (pos-sampler :sampler-2d)
     (normal-sampler :sampler-2d) (base-sampler :sampler-2d)
     (mat-sampler :sampler-2d) (diffuse-cube :sampler-cube) (depth :sampler-2d)
     (dfg :sampler-2d) (specular-cube :sampler-cube))
  (let* ((wpos (s~ (texture pos-sampler tc) :xyz))
	 (wnormal (normalize (s~ (texture normal-sampler tc) :xyz)))
	 (base-color (s~ (texture base-sampler tc) :xyz))
	 (material (texture mat-sampler tc))
	 (metallic (x material))
	 (roughness (y material))
	 (ao 1.0)
	 (light-inv-sqr-att-radius (/ 1 (pow light-radius 2)))
	 (n·v (+ (abs (dot wnormal wview-dir))
		 0.00001))) ;; biased to avoid artifacts)
    (setf gl-frag-depth (x (texture depth tc)))
    (+ ;; (punctual-light-luminance
       ;; 	wpos wnormal wview-dir n.v
       ;; 	base-color roughness metallic ao
       ;; 	light-origin light-radiance light-inv-sqr-att-radius)
     (* base-color (evaluate-ibl wnormal n·v roughness
				 diffuse-cube specular-cube dfg)))))

(def-g-> pbr-pass ()
  #'pass-through-vert my-pbr-analytic-light-frag)

;;----------------------------------------------------------------------

(defun-g my-pbr-post-prog-frag
    ((tex-coord :vec2) &uniform (linear-final :sampler-2d))
  (tone-map-uncharted2
   (s~ (texture linear-final tex-coord) :xyz)
   8s0
   1s0))

(def-g-> pbr-post-pass ()
  #'pass-through-vert my-pbr-post-prog-frag)

;;----------------------------------------------------------------------

(defun render-thing (thing camera)
  ;; - mipmaps
  ;; - sampling from sphere
  (let ((gb (get-gbuffer))
  	(pb (get-post-buff)))
    (using-camera camera
      (with-fbo-bound ((gbuffer-fbo gb))
      	(clear)
	(loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	   (map-g #'pack-gbuffer-pass (yaksha:mesh-stream mesh)
		  :model-space (in-space thing)
		  :base-tex (base-sampler thing)
		  :norm-tex (normal-sampler thing)
		  :mat-tex (material-sampler thing))))
      (let* ((light-pos (v! -4 10 0)))
	(with-fbo-bound ((post-buff-fbo pb))
	  (map-g #'pbr-pass *quad-stream*
		 :wview-dir (v! 0 0 -1)
		 :light-origin light-pos
		 :light-radius 200s0
		 :light-radiance (v! 2000.3 2000.3 2000.3)
		 :pos-sampler (gbuffer-pos-sampler gb)
		 :normal-sampler (gbuffer-norm-sampler gb)
		 :base-sampler (gbuffer-base-sampler gb)
		 :mat-sampler (gbuffer-mat-sampler gb)
		 :diffuse-cube light-probe-sampler
		 ;;:specular-cube light-probe-specular-sampler
		 :depth (gbuffer-depth-sampler gb)
		 :dfg dfg-sampler))))))

(defun post-proc (camera)
  (using-camera camera
    (let ((pb (get-post-buff)))
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
