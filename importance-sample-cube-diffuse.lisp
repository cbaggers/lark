(in-package :lark)


(defun-g get-sample ((i :uint) (n :uint))
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (v! (/ (float i) (float n))
      (radical-inverse-vdc i)))


(defun-g importance-sample-cos-dir ((u :vec2) (n :vec3))
  (let* ((up-vec (if (< (abs (z n)) 0.999)
		     (v! 0 0 1)
		     (v! 1 0 0)))
	 (tangent-x (normalize (cross up-vec n)))
	 (tangent-y (cross n tangent-x))
	 (u1 (x u))
	 (u2 (y u))
	 (r (sqrt u1))
	 (φ (* u2 +pi+ 2))
	 (pre-l (v! (* r (cos φ))
		    (* r (sin φ))
		    (sqrt (max 0s0 (- 1s0 u1)))))
	 (l (normalize (+ (* tangent-x (y pre-l))
			  (* tangent-y (x pre-l))
			  (* n (z pre-l)))))
	 (n·l (dot l n))
	 (pdf (* n·l +inv-pi+)))
    (values l n·l pdf)))


(defun-g integrate-diffuse-cube-ld ((n :vec3) (value-multiplier :float)
				    (cube :sampler-cube))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0)))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let ((eta (get-sample i sample-count)))
	   (multiple-value-bind (l n·l pdf)
	       (importance-sample-cos-dir eta n)
	     (%if (> n·l 0s0)
		  (let ((color (* (texture-lod cube l 0)
				  value-multiplier)))
		    (incf bdrf-accum color))))))
    (* bdrf-accum (/ 1s0 sample-count))))

(defun-g integrate-diffuse-2d-ld ((n :vec3) (value-multiplier :float)
				  (tex :sampler-2d))
  (let ((sample-count (uint 32))
	(bdrf-accum (v4! 0)))
    (for (i (uint 0)) (< i sample-count) (++ i)
	 (let ((eta (get-sample i sample-count)))
	   (multiple-value-bind (l n·l pdf)
	       (importance-sample-cos-dir eta n)
	     (%if (> n·l 0s0)
		  (let ((color (* (sample-equirectangular-tex tex l);; lod=0?
				  value-multiplier)))
		    (incf bdrf-accum color))))))
    (* bdrf-accum (/ 1s0 sample-count))))


(defun-g ld-diffuse-texture-cube ((uv :vec2) &uniform (value-multiplier :float)
				  (cube :sampler-cube))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions uv)
    (values (integrate-diffuse-cube-ld dir0 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir1 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir2 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir3 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir4 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir5 value-multiplier cube))))

(defun-g ld-diffuse-texture-2d ((uv :vec2) &uniform (value-multiplier :float)
				(tex :sampler-2d))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions uv)
    (values (integrate-diffuse-2d-ld dir0 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir1 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir2 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir3 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir4 value-multiplier tex)
	    (integrate-diffuse-2d-ld dir5 value-multiplier tex))))


(def-g-> diffuse-sample-hdr-cube ()
  #'pass-through-vert #'ld-diffuse-texture-cube)

(def-g-> diffuse-sample-hdr-2d ()
  #'pass-through-vert #'ld-diffuse-texture-2d)
