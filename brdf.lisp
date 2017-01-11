(in-package :lark)

;;----------------------------------------------------------------------

(defun-g fresnel-schlick ((f0 :vec3) (f90 :float) (u :float))
  (+ f0
     (* (- (v3! f90) f0)
	(pow (- 1s0 u) 5s0))))

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

(defun-g ggx-distribution ((n·h :float) (m :float))
  (let* ((m² (* m m))
	 (f (+ 1.0 (* (- (* n·h m²)
			 n·h)
		      n·h)))
	 (f² (* f f)))
    ;; we don't divide by +pi+ here as we will do that in the main brdf function
    (/ m² f²)))

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
