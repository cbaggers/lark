;;
;; Burley's diffuse
(defun fd (α ρ θd n·l n·v)
  ;; α - roughness
  ;; ρ - diffuse reflectance
  ;; θd -
  (let* ((ρ/π (/ ρ +pi+))
         (fd90 (+ 0.5 (* (expt (cos θd) 2) α)))
         (sat-n·l (saturate n·l))
         (sat-n·v (saturate n·v)))
    (* ρ/π
       (+ 1 (* fd90 (expt (- 1 sat-n·l) 5)))
       (+ 1 (* fd90 (expt (- 1 sat-n·v) 5))))))

;;
;; frostbite only tries to 'preserve' energy not 'conserve' it.
;; The former meaning (< (+ specular diffuse) 1)
;; and the latter meaning (= (+ specular diffuse) 1)


;;
;; I feel like I missing something huge from the following:
;; (page 13)
;;
;; Smoothness Defines the roughness of an object. We chose to use
;; smoothness instead of roughness because mapping white to smooth values
;; is more intuitive for artists, and they were already used to it with
;; Frostbite’s non-PBR material model. Similar to Burley’s presentation,
;; smoothness is remapped into perceptually linear smoothness (- 1 αlin).
;;
;; MetalMask Defines the “metalness” or conductivity of a surface
;; (i.e. dielectric/conductor), as in Burley’s presentation. We named it
;; metal mask to suggest the binary nature of this variable to artists.
;;
;; Reflectance Defines the Fresnel reflectance value at normal incidence
;; (f0) into an artist-friendly range for dielectric materials
;; (i.e. MetalMask < 1). The lower part of this attribute defines a
;; micro-specular occlusion term used for both dielectric and metal
;; materials.

;; For the smoothness remapping, we analyzed different remapping
;; functions and tested them with artists. Similarly to Burley’s
;; presentation, we have chosen the “squaring” remapping which seems the
;; most pleasing one for our artists.

;; For the reflectance, we chose the following remapping function:
;; f0 = (* 0.16 reflectance²)
;; The goal was to map f 0 onto a range which could include the high
;; Fresnel values of gemstones, with the constraint of remapping RGB 128
;; onto the common dielectric 4% reflectance.
;; For gemstones, f 0 goes approximately from 8% for ruby to 17% for
;; diamond. We chose to limit the function to 16% as an
;; approximation. Comparisons with common values are shown in Figure
;; 13. In practice, with our real-time limitations, a variation in f 0 of
;; 1% or 2% is barely noticeable 5 . The fast growing values above 4% fit
;; well with this. We also use the lower part < 2% (water reflectance) to
;; provide
