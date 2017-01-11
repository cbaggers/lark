(in-package :lark)

(defun-g calc-irr ((normal :vec3) (env-map :sampler-2d))
  (let* ((up (v! 0 1 0))
         (right (normalize (cross up normal)))
         (up (cross normal right))
         (sampled-color (v! 0 0 0))
         (index 0s0))
    ;; loop over hemisphere
    (for (φ 0s0) (< φ 6.283) (setq φ (+ φ 0.025))
         (for (θ 0s0) (< θ 1.57) (setq θ (+ θ 0.1))
              (let* ((temp (+ (* (cos φ) right)
                              (* (sin φ) up)))
                     (sample-vec (+ (* normal (cos θ))
                                    (* temp (sin θ))))
                     ;; change sample-vec in the form below to normal to
                     ;; prove that both uv->cube-map-directions and
                     ;; sample-equirectangular-tex are working.
                     (sample (s~ (sample-equirectangular-tex
                                  env-map sample-vec)
                                 :xyz))
                     (final (* sample (cos θ) (sin θ))))
                (setf sampled-color (+ sampled-color final))
                (setf index (+ index 1)))))
    (v! (/ (* sampled-color +pi+) index) 1)))

#+t
(setf *regen-light-probe* t)

(defun-g learn-ibl-convolve-envmap ((tc :vec2) &uniform (env-map :sampler-2d))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions tc)
    (values (calc-irr dir0 env-map)
	    (calc-irr dir1 env-map)
	    (calc-irr dir2 env-map)
	    (calc-irr dir3 env-map)
	    (calc-irr dir4 env-map)
	    (calc-irr dir5 env-map))))


(def-g-> learn-ibl-convolve-pass ()
  (pass-through-vert g-pt)
  (learn-ibl-convolve-envmap :vec2))
