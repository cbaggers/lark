(in-package :lark)

;;----------------------------------------------------------------------

(defun-g ggx-distribution ((n·h :float) (m :float))
  (let* ((m² (* m m))
         (f (+ 1.0 (* (- (* n·h m²)
                         n·h)
                      n·h)))
         (f² (* f f)))
    ;; we don't divide by +pi+ here as we will do that in the main brdf function
    (/ m² f²)))

(defun-g specular-brdf ((n·v :float) (l·h :float) (n·h :float) (n·l :float)
                        (h :vec3) (f0 :vec3) (f90 :float) (roughness :float))
  (let ((f (fresnel-schlick f0 f90 l·h))
        (g (ggx-geom-smith-correlated n·v n·l roughness))
        (d (ggx-distribution n·h roughness)))
    (/ (* d g f) +pi+)))

(defun-g punctual-light ((albedo :vec3) (n·v :float) (half-vec :vec3)
                         (l·h :float) (n·h :float) (n·l :float)
                         (linear-roughness :float) (roughness :float)
                         (metallic :float))
  (let* ((fd (/ (disney-diffuse n·v n·l l·h linear-roughness) +pi+))
         (f0 (mix (v3! 0.04) albedo metallic))
         (f90 (saturate (* 50s0 (dot f0 (v3! 0.33)))))
         (fr (specular-brdf n·v l·h n·h n·l half-vec f0 f90 roughness))
         (brdf (+ (* albedo fd) fr)))
    (* brdf n·l
       ;;attenuation
       ;;light-color
       )))

;;----------------------------------------------------------------------

(defun-g inverse-square ((x :float))
  (/ 1f0 (* x x)))

(defun-g cook-torrance-brdf ((world-pos :vec3)
                             (normal :vec3)
                             (albedo :vec3)
                             (metallic :float)
                             (roughness :float)
                             (f0 :vec3) ;; base reflectance
                             ;;
                             (light-pos :vec3)
                             (light-color :vec3)
                             ;;
                             (view-dir :vec3))
  (let* ((light-dir (- light-pos world-pos))
         (normalized-light-dir (normalize light-dir))
         (half-vec (normalize (+ view-dir normalized-light-dir)))
         ;;
         (distance (length light-dir))
         (attenuation (* (/ 1f0 distance)
                         10))
         (radiance (* light-color attenuation))
         ;;
         (h·n (max (dot half-vec normal) 0f0))
         (h·v (max (dot half-vec view-dir) 0f0))
         (l·n (max (dot normal normalized-light-dir) 0f0))
         (n·v (max (dot normal view-dir) 0f0))
         ;;
         (fresnel (fresnel-schlick f0 h·v))
         ;;
         (diffuse (lambertian-diffuse albedo))
         (specular (cook-torrance-specular
                    #'(distribution-ggx-trowbridge-reitz :float :float)
                    fresnel
                    #'(geometry-smith :float :float :float)
                    h·n
                    l·n
                    n·v
                    roughness))

         ;;
         (specular-component fresnel) ;; ks
         (diffuse-component (* (- (v3! 1f0) specular-component) ;; kd
                               (- 1f0 metallic)))
         ;; metals have no diffuse ↑↑
         ;;
         (lₒ (* (+ (* diffuse diffuse-component) specular)
                radiance ;; attenuated light color
                l·n))) ;; incident angle
    lₒ))

(defun-g lambertian-diffuse ((albedo :vec3))
  ;; flambert in the literature
  (* albedo inv-pi-f))

(defun-g cook-torrance-specular
    ((distribution (function (:float :float) :float)) ;; d
     (fresnel :vec3)
     (geometry (function (:float :float :float) :float)) ;; g
     (h·n :float) ;; (saturate (dot normal half-vec))
     (l·n :float)
     (n·v :float)
     (remapped-α :float) ;; k [α was a measure of roughness]
     )
  ;; fcook-torrance in the literature
  (/ (* (funcall distribution h·n remapped-α)
        fresnel
        (funcall geometry n·v l·n remapped-α))
     (+ (* 4 n·v l·n)
        ;; this ensures no divide by zero
        ;; ↓↓
        0.001)))

;;
;; A note on α
;;
;; α is some measure of roughness, but the value of α might differ based on how
;; your engine translates roughness to α.
;;
;; {TODO} flesh this note out with details
;;

(defun-g distribution-ggx-trowbridge-reitz
    ((h·n :float) ;; (saturate (dot normal half-vec))
     (α :float)) ;; measure of roughness
  ;;
  ;; normal distribution function
  ;; D in the DFG portion of the cook-torrance-specular BRDF
  (let* ((α² (* α α)) ;; will be our nonimator
         (h·n² (* h·n h·n))
         (denominator (+ (* h·n² (- α² 1f0))
                         1f0))
         (denominator (* pi-f denominator denominator)))
    (/ α² denominator)))


(defun-g α-remap-direct-light ((α :float)) ;; a measure of roughness
  (let* ((α+1 (+ α 1))
         (α+1² (* α+1 α+1)))
    (/ α+1² 8)))

(defun-g α-remap-ibl ((α :float)) ;; a measure of roughness
  (* 0.5 α α))

(defun-g geometry-schlick-ggx ((n·v :float) ;; (dot normal view-dir)
                               (remapped-α :float)) ;; k
  ;; note (α was a measure of roughness)
  "Aproximation of how much obstruction of light due to microfacet
   cross shadowing is occuring"
  (/ n·v
     (+ (* n·v (- 1f0 remapped-α)) remapped-α)))


(defun-g geometry-smith ((n·v :float)
                         (l·n :float)
                         (remapped-α :float)) ;; k
  ;; note (α was a measure of roughness)
  "Geometry function
   G in the DFG portion of the cook-torrance-specular BRDF

   geometry-schlick-ggx does the real work here but we need to
   take into account geometry obsctruction both from the light
   to the surface and from the surface to the viewer"
  (let* ((ggx-0 (geometry-schlick-ggx n·v remapped-α))
         (ggx-1 (geometry-schlick-ggx l·n remapped-α)))
    (* ggx-0 ggx-1)))


(defun-g geometry-smith ((n·v :float)
                         (l·n :float)
                         (α-remap-func (function (:float) :float))
                         (α :float))  ;; a measure of roughness
  "Geometry function
   G in the DFG portion of the cook-torrance-specular BRDF

   geometry-schlick-ggx does the real work here but we need to
   take into account geometry obsctruction both from the light
   to the surface and from the surface to the viewer"
  (geometry-smith n·v l·n (funcall α-remap-func α)))

(defun-g simple-f0 ((albedo :vec3)
                    (metallic :float))
  "One possible implementation of transforming f0 to allow the
   same metric to work with metals & dielectrics"
  (mix (vec3 0.04) albedo metallic))
