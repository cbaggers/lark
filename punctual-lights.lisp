(in-package :lark)

;;----------------------------------------------------------------------

(defun-g simple-f0 ((albedo :vec3)
                    (metallic :float))
  "One possible implementation of transforming f0 to allow the
   same metric to work with metals & dielectrics

   impl: `(mix (vec3 0.04) albedo metallic)`"
  (mix (vec3 0.04) albedo metallic))

;;----------------------------------------------------------------------
;;
;; A note on α
;;
;; α is some measure of roughness, but the value of α might differ based on how
;; your engine translates roughness to α.
;;
;; {TODO} flesh this note out with details
;;

(defun-g α-remap-direct-light ((α :float)) ;; a measure of roughness
  (let* ((α+1 (+ α 1))
         (α+1² (* α+1 α+1)))
    (/ α+1² 8)))

(defun-g α-remap-ibl ((α :float)) ;; a measure of roughness
  (* 0.5 α α))

;;----------------------------------------------------------------------
;; Diffuse

(defun-g lambertian-diffuse ((albedo :vec3))
  ;; flambert in the literature
  (* albedo inv-pi-f))

;;----------------------------------------------------------------------
;; Specular

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


(defun-g cook-torrance-specular
    ((fresnel :vec3)
     (h·n :float) ;; (saturate (dot normal half-vec))
     (l·n :float)
     (n·v :float)
     (α :float)) ;; a measure of roughness

  ;; fcook-torrance in the literature
  (/ (* (distribution-ggx-trowbridge-reitz h·n (* α α)) ;; d
        fresnel ;; f
        (geometry-smith n·v l·n (α-remap-direct-light α))) ;; g
     (+ (* 4 n·v l·n)
        ;; this ensures no divide by zero
        ;; ↓↓
        0.001)))

;;----------------------------------------------------------------------

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
         (attenuation (inverse-square distance))
         (radiance (* light-color attenuation))
         ;;
         (h·n (saturate (dot half-vec normal)))
         (h·v (saturate (dot half-vec view-dir)))
         (l·n (saturate (dot normal normalized-light-dir)))
         (n·v (saturate (dot normal view-dir)))
         ;;
         (fresnel (fresnel-schlick f0 h·v))
         ;;
         (diffuse (lambertian-diffuse albedo))
         (specular (cook-torrance-specular
                    fresnel
                    h·n
                    l·n
                    n·v
                    roughness))
         ;;
         (specular-component fresnel) ;; ks
         (diffuse-component (* (- (v3! 1f0) specular-component) ;; kd
                               (- 1f0 metallic))) ;; ← metals have no diffuse
         (lₒ (* (+ (* diffuse diffuse-component) specular)
                radiance ;; attenuated light color
                l·n))) ;; incident angle
    lₒ))
