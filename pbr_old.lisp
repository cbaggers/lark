(in-package :assurance)

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

(defun-g saturate ((val :float))
  (clamp val 0s0 1s0))

;; phong (lambertian) diffuse term
(defun-g lambertian-diffuse ()
  (/ 1s0 +pi+))

;; compute fresnel specular factor for given base specular and product
;; product could be NdV or VdH depending on used technique
(defun-g fresnel-factor ((f0 :vec3) (product :float))
  (mix f0 (v! 1 1 1) (pow (- 1.01 product) 5)))

;;----------------------------------------------------------------------
;; following functions are copies of UE4
;; for computing cook-torrance specular lighting terms

(defun-g d-blinn ((roughness :float) (ndh float))
  (let* ((m (* roughness roughness
	       roughness roughness))
	 (n (- (/ 2s0 m) 2s0)))
    (* (/ (+ n 2s0) (* 2s0 +pi+)) (pow ndh n))))


(defun-g d-beckmann ((roughness :float) (ndh float))
  (let* ((m (* roughness roughness
	       roughness roughness))
	 (ndh2 (* ndh ndh)))
    (/ (exp (/ (- ndh2 1s0) (* m ndh2)))
       (* +pi+ m ndh2 ndh2))))


(defun-g d-ggx ((roughness :float) (ndh float))
  (let* ((m (* roughness roughness
	       roughness roughness))
	 (d (* (- (* ndh m) ndh) (+ ndh 1s0))))
    (/ m (* +pi+ d d))))


(defun-g g-shlick ((roughness :float) (ndh float) (ndl :float))
  (let* ((k (* roughness roughness 0.5))
	 (v (+ (* ndv (- 1s0 k)) k))
	 (l (+ (* ndl (- 1s0 k)) k)))
    (/ 0.25 (* v l))))

;;----------------------------------------------------------------------
;; cook-torrance specular calculation

(defun-g cooktorrance-specular-blinn ((ndl :float) (ndv :float) (ndh :float)
				      (specular :vec3) (roughness :float))
  (let* ((d (d-blinn roughness ndh))
	 (g (g-shlick roughness ndv ndl))
	 (rim (mix (- 1s0 (* roughness (w material?) 0.9)) 1s0 ndv)))
    (* (/ 1s0 rim) specular g d)))

(defun-g cooktorrance-specular-beckman ((ndl :float) (ndv :float) (ndh :float)
					(specular :vec3) (roughness :float))
  (let* ((d (d-beckman roughness ndh))
	 (g (g-shlick roughness ndv ndl))
	 (rim (mix (- 1s0 (* roughness (w material?) 0.9)) 1s0 ndv)))
    (* (/ 1s0 rim) specular g d)))

(defun-g cooktorrance-specular-ggx ((ndl :float) (ndv :float) (ndh :float)
				    (specular :vec3) (roughness :float))
  (let* ((d (d-ggx roughness ndh))
	 (g (g-shlick roughness ndv ndl))
	 (rim (mix (- 1s0 (* roughness (w material?) 0.9)) 1s0 ndv)))
    (* (/ 1s0 rim) specular g d)))

;;----------------------------------------------------------------------

;; Approach
;; --------
;; - diffuse brdf - lambertian
;; - specular brdf
;;   - D - ggx/trowbridge-reitz
;;   - G - Schlick with unreal modification for analytics lights
;;   - F - schlick fresnel

;; gbuffer layout (crappy but fine for testing)
;; --------------------------------------------
;; rgb16f pos
;; rgb16f normal
;; rgb8 base-color
;; rgb8 metallic, AO, roughness

(labels ((brdf (ωi ωo)
	   (+ (* kd (lambertian))
	      (* ks (cook-torrance ωi ωo)))))
  (let ((radiance-out (* (brdf ωi ωo) (irradiance point ωi) (dot normal ωi))))
    ...))

(defun cook-torrance ()
  (/ (* (D) (F) (G))
     (* 4 (dot ωo n) (dot ωi n))))

(defun-g my-pbr-ambiant-frag
    ((tex-coord :vec2)
     &uniform (pos-sampler :sampler-2d) (normal-sampler :sampler-2d)
     (base-color-sampler :sampler-2d) (material-sampler :sampler-2d))
  (let ((pos (texture pos-sampler tex-coord))
	(normal (texture normal-sampler tex-coord))
	(base-color (texture base-color-sampler tex-coord)))
    (bind-vec (metallic ao roughness) (texture material tex-coord)
      )))

(defun-g my-pbr-analytic-light-frag
    ((tex-coord :vec2) (light-origin :vec3) (light-radius :float)
     (light-radiance :vec3)
     &uniform (diffuse :sampler-2d) (normal-map :sampler-2d))
  )

(defun-g my-pbr-post-prog-frag
    ((tex-coord :vec2) &uniform (linear-final :sampler-2d))
  (linear->non-linear
   (tone-map
    (texture linear-final tex-coord))))



(defun-g pbr-frag-base ((view-space-pixel-pos :vec3) (normal :vec3)
			(binormal :vec3) (tex-coord :vec2)
			&uniform (diffuse :sampler-2d) (normal-map :sampler-2d)
			(spec? :sampler-2d)
			;; IBL BRDF normalization precalculated tex
			(iblbrdf :sampler-2d)
			(env-diffuse :sampler-cube)) ;; prefiltered env
  (let ((light-pos (v! -2 3 -2 1))
	;; point light direction to point in view space
	(local-light-pos &&&&&&)
	(x0 (- local-light-pos view-space-pixel-pos))
	;; light attenuation
	(a (/ 20s0 (dot x0 x0)))
	;; L, V, H vectors
	(l (normalize (- local-light-pos view-space-pixel-pos)))
	(v (normalize (- view-space-pixel-pos)))
	(h (normalize (+ l v)))
	(nn (normalize normal))
	(nb (normalize binormal))
	(tbn (m3:from-columns nb (cross nn nb) nn))
	;;normal map
	(n (* tbn (- (* (s~ (texture normal-map tex-coord) :xyz) 2) 1s0)))
	;;albedo/specular base
	(base (s~ (texture diffuse tex-coord) :xyz))
	;; roughness
	(roughness (* (y (texture spec? tex-coord)) (y material?)))
	;; mix between metal and non-metal material, for non-metal
	;; constant base specular factor of 0.04 grey is used
	(specular (mix (v! 0.04 0.04 0.04) base (x material?))) ;; (x material?) == metallic
	;; diffuse IBL term
	;; I know that my IBL cubemap has diffuse pre-integrated value in 10th
	;; MIP level actually level selection should be tweakable or from
	;; separate diffuse cubemap
	(tnrm (transpose normal_matrix?))
	(env-diff (s~ (texture-lod env-diffuse (* tnrm n) 10) :xyz))
	;; specular IBL term
	;; 11 magic number is total MIP levels in cubemap, this is simplest way
	;; for picking MIP level from roughness value (but it's not correct,
	;; however it looks fine)
	(refl (* tnrm (reflect (- v) n)))
	(env-spec (s~ (texture-lod
		       env-diffuse refl
		       (max (* roughness 11s0)
			    (y (texture-query-lod env-diffuse refl))))
		      :xyz))
	;; compute material reflectance
	(ndl (max 0.0 (dot n l)))
	(ndv (max 0.001 (dot n v)))
	(ndh (max 0.001 (dot n h)))
	(hdv (max 0.001 (dot h v)))
	(ldv (max 0.001 (dot l v)))
	;; fresnel term
	(spec-fresnel (fresnel-factor specular HdV))
	(spec-ref (cooktorrance-specular-beckman
		   ndl ndv ndh specfresnel roughness))
	(spec-ref (* spec-ref (v! ndl ndl ndl)))
	;; diffuse
	(diffref (* (- (v! 1 1 1) spec-fresnel) (lambertian-diffuse) ndl))
	;; compute lighting
	(reflected-light (v! 0 0 0))
	;; point light
	(light-color (* a (v! 1 1 1)))
	(diffuse-light (+ (v! 0 0 0) (* diffref light-color))) ;; the empty vec is the constant ambient
	(reflected-light (+ reflected-light (* specref light-color)))
	;; IBL lighting
	(brdf (s~ (texture iblbrdf (v! roughness (- 1.0 NdV))) :xy))
	(iblspec (min (v! 0.99 0.99 0.99)
		      (+ (* (fresnel-factor specular ndv) (x brdf)) (y brdf))))
	(reflected-light (+ reflected-light (* iblspec envspec)))
	(diffuse-light (+ diffuse-light (* envdiff (/ 1.0 PI))))
	;; final result
	(result (+ (* diffuse-light (mix base (v! 0 0 0) (x metallic)))
		   reflected-light)))
    (v! result 1)))
