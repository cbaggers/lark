(in-package :lark)
(in-readtable fn:fn-reader)


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
     (base-color :vec3) (linear-roughness :float) (metallic :float) (ao :float)
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
       attenuation)))

;;----------------------------------------------------------------------


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
    ;; usually the following is (v! .. (w pre-ld)) but we arent using alpha
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
    ;; as with specular, we arent using alpha
    (* (s~ diffuse-lighting :xyz) diff-f)))



;; I've been messing with this
(defun-g evaluate-ibl ((wnormal :vec3) (wview-dir :vec3) (n·v :float)
		       (linear-roughness :float) (cube :sampler-cube)
		       (specular-cube :sampler-cube) (dfg :sampler-2d)
		       (base-color :vec3) (metallic :float))
  (let* (;; these 2 are copied from the analytical light
	 ;; could be wrong assumption. For example it looked way better as
	 ;; (roughness linear-roughness)
	 ;; (linear-roughness (sqrt roughness))
	 (roughness (* linear-roughness linear-roughness))
	 (albedo (mix base-color (v3! 0) metallic))

	 ;; lighting time
	 (fd (evaluate-ibl-diffuse wnormal wview-dir n·v roughness cube dfg))
	 (fr (let* (;; this should be the reflected vec..but I was lazy
		    (r wnormal)
		    ;; again stole these two from the analytical light
		    (f0 (mix (v3! 0.04) base-color metallic))
		    (f90 (saturate (* 50s0 (dot f0 (v3! 0.33))))))
	       ;; the 7 is the num of mipmaps, I shoved that there as I dont
	       ;; know what I'm doing
	       (evaluate-ibl-specular wnormal r n·v linear-roughness roughness
				      f0 f90 specular-cube 7 dfg))))
    ;; again taken from analytical. who knows what to do here?
    (+ (* albedo fd) (/ fr 100))))


;;----------------------------------------------------------------------


(defun-g material-pbr-pass
    ((tc :vec2) &uniform (wview-dir :vec3) (light-origin :vec3)
     (light-radius :float) (light-radiance :vec3) (pos-sampler :sampler-2d)
     (normal-sampler :sampler-2d) (base-sampler :sampler-2d)
     (mat-sampler :sampler-2d) (diffuse-cube :sampler-cube)
     (dfg :sampler-2d) (specular-cube :sampler-cube) (depth :sampler-2d))
  ;; This is the real meat :)
  (let* (;; unpack the gbuffer
	 (wpos (s~ (texture pos-sampler tc) :xyz))
	 (wnormal (normalize (s~ (texture normal-sampler tc) :xyz)))
	 (base-color (s~ (texture base-sampler tc) :xyz))
	 (material (texture mat-sampler tc))
	 (metallic (x material))
	 (roughness (y material)) ;; [try sqrt'ing this guy]
	 (ao 1.0)

	 ;; is this right?
	 (light-inv-sqr-att-radius (/ 1 (pow light-radius 2)))

	 ;; this guy is needed everywhere
	 ;; the 0.00001 is the bias to avoid artifacts
	 (n·v (+ (abs (dot wnormal wview-dir)) 0.00001))

	 ;; sum the analytical & ibl light
	 (lin-final (+ (punctual-light-luminance
		       	wpos wnormal wview-dir n·v
		       	base-color roughness metallic ao
		       	light-origin light-radiance light-inv-sqr-att-radius)
		       (evaluate-ibl
		       	wnormal wview-dir n·v roughness
		       	diffuse-cube specular-cube dfg
		       	base-color metallic))))

    ;; set the depth so the skybox works
    (setf gl-frag-depth (x (texture depth tc)))

    ;; tonemap and make non-linear again
    (tone-map-uncharted2 lin-final 6s0 1s0)))


(def-g-> pbr-pass ()
  #'pass-through-vert #'material-pbr-pass)

;;----------------------------------------------------------------------

(defun render-thing (thing camera render-state)
  (with-slots (light-probe-diffuse light-probe-specular gbuffer dfg)
      render-state

    ;; Using the camera makes sure the space-system knows which space is
    ;; cam space and also sets the viewport
    (using-camera camera
      ;; bind the gbuffer fbo (no need to set the viewport again)
      (with-fbo-bound ((fbo gbuffer) :with-viewport nil)
      	;; clear the bound fbo
      	(clear))
      populate the gbuffer
      (loop :for mesh :in (yaksha:model-meshes (model thing)) :do
	 (map-g #'pack-gbuffer-pass (yaksha:mesh-stream mesh)
		:model-space (model-space thing)
		:base-tex (base-sampler thing)
		:norm-tex (normal-sampler thing)
		:metallic-tex (metallic-sampler thing)
		:roughness-tex (roughness-sampler thing)))

      ;; do the pbr pass, rendering the the default fbo
      (let* ((light-pos (v! 0 1000 -160)))
      	(map-g #'pbr-pass *quad-stream*
      	       :wview-dir (v! 0 0 -1)
      	       :light-origin light-pos
      	       :light-radius 200s0
      	       :light-radiance (v! 2000.3 2000.3 2000.3)
      	       :pos-sampler (pos-sampler gbuffer)
      	       :normal-sampler (norm-sampler gbuffer)
      	       :base-sampler (base-sampler gbuffer)
      	       :mat-sampler (mat-sampler gbuffer)
      	       :diffuse-cube (sampler light-probe-diffuse)
      	       :specular-cube (sampler light-probe-specular)
      	       :dfg (sampler dfg)
      	       :depth (depth-sampler gbuffer)))
      )))


(defun render (camera game-state)
  (let* ((render-state (render-state game-state)))
    (with-slots (dfg light-probe-diffuse light-probe-specular env-map)
	render-state

      ;; clear the default fbo
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      ;; ;; populate the dfg LUT
      ;; (map-g-into (fbo dfg)
      ;; 		  #'dfg-texture-pass *quad-stream*)

      ;; ;; precalc the diffuse portion of the IBL
      ;; (map-g-into (fbo light-probe-diffuse)
      ;; 		  #'diffuse-sample-hdr-cube *quad-stream*
      ;; 		  :value-multiplier 1s0 :cube env-map)

      ;; (generate-mipmaps (cube light-probe-diffuse))

      ;; ;; precalc the specular portion of the IBL
      ;; (map-g-into (fbo light-probe-specular)
      ;; 		  #'specular-sample-hdr-cube *quad-stream*
      ;; 		  :value-multiplier 1s0 :cube env-map :roughness 0.1)

      ;; (generate-mipmaps (cube light-probe-specular))

      ;; render all the shiz
      (map nil λ(render-thing (update-thing _) camera render-state)
	   (things *game-state*))

      ;; ;; skybox time
      ;; (render-sky camera render-state)

      ;; flip dem buffers
      (swap))))
