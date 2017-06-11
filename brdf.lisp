(in-package :lark)

;;----------------------------------------------------------------------

(defun-g fresnel-schlick ((f0 :vec3)
                          (cosθ :float))
  (+ f0
     (* (- (v3! 1f0) f0)
        (pow (- 1s0 cosθ) 5s0))))

(defun-g fresnel-schlick ((f0 :vec3)
                          (cosθ :float)
                          (f90 :float))
  "Calculates the fresnel aproximatation using the shlick method.

   f0 is the base-reflectivity of the material. It is a vec3 as metalic objects
   have tint.

   The optional f90 argument is the amount of reflectance at 90 degrees. This
   is technically 1f0 but can be changed if required by your use case."
  (+ f0
     (* (- (v3! f90) f0)
        (pow (- 1s0 cosθ) 5s0))))

;; vec3 fresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness)
;; {
;;     return F0 +
;;            (max(vec3(1.0 - roughness), F0) - F0) *
;;             pow(1.0 - cosTheta, 5.0);
;; }

(defun-g fresnel-schlick-roughness ((f0 :vec3)
                                    (cosθ :float)
                                    (roughness :float))
  (+ f0
     (* (- (max (v3! (- 1f0 roughness)) f0)
           f0)
        (pow (- 1s0 cosθ) 5s0))))

;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------

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


;; http://graphicrants.blogspot.com.au/2013/08/specular-brdf-reference.html
(defun-g ggx ((a :float) (n·v :float))
  (let ((k (* a 0.5)))
    (/ n·v (+ (* n·v (- 1 k)) k))))

;; http://graphicrants.blogspot.com.au/2013/08/specular-brdf-reference.html
(defun-g g-smith ((roughness :float) (n·v :float) (n·l :float))
  (let ((α (* roughness roughness)))
    (* (ggx α n·v)
       (ggx α n·l))))
