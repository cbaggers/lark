(in-package :lark)

(defun-g radical-inverse-vdc ((bits :uint))
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (let ((bits (bit-ior (<< bits (uint 16)) (>> bits (uint 16))))
	(bits (bit-ior (<< (bit-and bits (uint #x55555555)) (uint 1))
		       (>> (bit-and bits (uint #xAAAAAAAA)) (uint 1))))
	(bits (bit-ior (<< (bit-and bits (uint #x33333333)) (uint 2))
		       (>> (bit-and bits (uint #xCCCCCCCC)) (uint 2))))
	(bits (bit-ior (<< (bit-and bits (uint #x0F0F0F0F)) (uint 4))
		       (>> (bit-and bits (uint #xF0F0F0F0)) (uint 4))))
	(bits (bit-ior (<< (bit-and bits (uint #x00FF00FF)) (uint 8))
		       (>> (bit-and bits (uint #xFF00FF00)) (uint 8)))))
    ;;                  ↓ 0x100000000 ↓
    (* (float bits) 2.3283064365386963e-10)))


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
	 (n·l (dot n l))
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


(defun-g uv->cube-map-directions ((uv :vec2))
  (let ((scaled-uv (v! (* (- (x uv) 0.5) 2)
		       (* (- (y uv) 0.5) 2 -1s0))))
    (values
     (v! 1s0 (y scaled-uv) (- (x scaled-uv)))
     (v! -1s0 (y scaled-uv) (x scaled-uv))
     (v! (x scaled-uv) 1s0 (- (y scaled-uv)))
     (v! (x scaled-uv) -1s0 (y scaled-uv))
     (v! (x scaled-uv) (y scaled-uv) 1s0)
     (v! (- (x scaled-uv)) (y scaled-uv) -1s0))))


(defun-g ld-diffuse-texture ((uv :vec2) &uniform (value-multiplier :float)
			     (cube :sampler-cube))
  (multiple-value-bind (dir0 dir1 dir2 dir3 dir4 dir5)
      (uv->cube-map-directions uv)
    (values (integrate-diffuse-cube-ld dir0 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir1 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir2 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir3 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir4 value-multiplier cube)
	    (integrate-diffuse-cube-ld dir5 value-multiplier cube))))


(def-g-> diffuse-sample-hdr-cube ()
  #'pass-through-vert #'ld-diffuse-texture)
