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
