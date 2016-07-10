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
;;
