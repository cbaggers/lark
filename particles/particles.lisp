(in-package :lark)

(defvar *fullscreen-quad*)
(defvar *particle-stream*)
(defparameter *particle-resolution* '(1024 1024))
(defparameter *starting-positions* nil)
(defparameter *starting-velocities*
  (make-c-array (make-array *particle-resolution* :initial-element (v! 0 0 0))
		:element-type :half-vec3))

(deftclass particle-gbuffer
  (positions (make-texture *starting-positions*
			   :element-type :rgb32f))
  (positions-fbo nil)
  (velocities (make-texture *starting-velocities*
			    :element-type :rgb16f))
  (velocities-fbo nil))

(deftclass (particle-system (:constructor %make-particle-system))
  (front-gbuffer (make-particle-gbuffer))
  (back-gbuffer (make-particle-gbuffer) :type particle-gbuffer)
  (front-to-back t :type boolean))

(defun make-particle-system ()
  (let ((result (%make-particle-system)))
    (let ((front (particle-system-front-gbuffer result))
	  (back (particle-system-back-gbuffer result)))
      (setf (particle-gbuffer-positions-fbo front)
	    (make-fbo `(:c ,(particle-gbuffer-positions front)))
	    (particle-gbuffer-velocities-fbo front)
	    (make-fbo `(:c ,(particle-gbuffer-velocities front))))
      (setf (particle-gbuffer-positions-fbo back)
	    (make-fbo `(:c ,(particle-gbuffer-positions back)))
	    (particle-gbuffer-velocities-fbo back)
	    (make-fbo `(:c ,(particle-gbuffer-velocities back))))
      result)))

(defun reset-particle-system (sys)
  (let ((front (particle-system-front-gbuffer sys))
	(back (particle-system-back-gbuffer sys)))
    (push-g *starting-positions*
	    (particle-gbuffer-positions-fbo front))
    (push-g *starting-velocities*
	    (particle-gbuffer-positions-fbo back))
    sys))

(defmethod free ((object particle-system))
  (free (particle-system-front-gbuffer object))
  (free (particle-system-back-gbuffer object)))

(defmethod free ((object particle-gbuffer))
  (free (particle-gbuffer-positions object))
  (free (particle-gbuffer-velocities object)))

;;----------------------------------------------------------------

(defun init-particles ()
  (destructuring-bind (verts indices-for-1) (dendrite.primitives:plain-data
					     :normals nil)
    (setf *fullscreen-quad*
	  (make-buffer-stream
	   (make-gpu-array verts :element-type 'cepl:g-pt)
	   :index-array (make-gpu-array indices-for-1 :element-type :ushort)))
    (setf *particle-stream*
	  (apply #'make-particle-stream *particle-resolution*))
    (let ((arr (make-c-array nil :dimensions *particle-resolution*
			     :element-type :vec3)))
      (labels ((init (ptr x y)
		 (declare (ignore x y))
		 (cepl.types.foreign:vec3-to-foreign
		  ptr (- (random 20.0) 10) (- (random 20.0) 10) 0s0)))
	(setf *starting-positions* (across-c-ptr #'init arr))))
    t))

;;----------------------------------------------------------------

(defun-g particle-vert ((vert cepl:g-pt))
  (values (v! (pos vert) 1) (cepl:tex vert)))

(defun-g update-particle-positions ((tex-coord :vec2)
				    &uniform (positions :sampler-2d)
				    (velocities :sampler-2d))
  (let ((position (texture positions tex-coord))
	(velocity (texture velocities tex-coord)))
    (+ (v! (s~ position :xy) -20 0) velocity)))

(def-g-> move-particles ()
  #'particle-vert #'update-particle-positions)

;;----------------------------------------------------------------

(defun-g update-particle-velocities ((tex-coord :vec2)
				     &uniform (positions :sampler-2d)
				     (velocities :sampler-2d))
  (let ((position (texture positions tex-coord))
	(velocity (texture velocities tex-coord)))
    (* (normalize (- (v! 0 0 -20 0) position)) 0.0001)))

(def-g-> update-velocities ()
  #'particle-vert #'update-particle-velocities)

;;----------------------------------------------------------------

(defun-g place-particle ((vert :vec4) &uniform (positions :sampler-2d))
  (let* ((pos-index (v!int (int (floor (v:z vert))) (int (floor (v:w vert)))))
	 (particle-position (texel-fetch positions pos-index 0))
	 (corner-pos (v! (v:x vert) (v:y vert))))
    (values (in *clip-space*
	      (in *world-space*
		(sv! (+ (v! corner-pos 0 0)
		       (v! 0 0 -500 0)
		       (* 100 particle-position)))))
	    (* (+ corner-pos (v! 1 1)) 0.5))))

(defun-g place-particle-frag ((tex-coord :vec2))
  (v! 1 0 1 0))

(def-g-> draw-particles ()
  #'place-particle #'place-particle-frag)

;;----------------------------------------------------------------

(defun update-particles (particle-system)
  (let* ((f2b (particle-system-front-to-back particle-system))
	 (source (if f2b
		     (particle-system-front-gbuffer particle-system)
		     (particle-system-back-gbuffer particle-system)))
	 (destination (if f2b
			  (particle-system-back-gbuffer particle-system)
			  (particle-system-front-gbuffer particle-system))))
    (setf (particle-system-front-to-back particle-system) (not f2b))
    ;;
    (with-fbo-bound ((particle-gbuffer-velocities-fbo destination)
    		     :with-blending nil)
      (map-g #'update-velocities *fullscreen-quad*
    	     :positions (particle-gbuffer-positions source)
    	     :velocities (particle-gbuffer-velocities source)))
    ;;
    (with-fbo-bound ((particle-gbuffer-positions-fbo destination)
		     :with-blending nil)
      (map-g #'move-particles *fullscreen-quad*
	     :positions (particle-gbuffer-positions source)
	     :velocities (particle-gbuffer-velocities source)))
    ;;
    (with-viewport (current-viewport)
	(using-camera *current-camera*
	  (map-g #'draw-particles *particle-stream*
		 :positions (particle-gbuffer-positions destination))))))


;;----------------------------------------------------------------

;; (v! vert.x vert.y pos.u pos.v)

(defun make-particle-stream (size-x size-y)
  (let* ((quad-verts (list (v! -1.0 -1.0) (v! 1.0 -1.0)
			   (v! 1.0 1.0) (v! -1.0 1.0)))
	 (verts
	  (with-c-array
	      (arr (make-c-array nil :dimensions (* 4 size-x size-y)
				 :element-type :vec4))
	    (labels ((put (ptr index)
		       (multiple-value-bind (y x) (floor index size-x)
			 (let ((qv (elt quad-verts (mod x 4))))
			   (cepl.types.foreign:vec4-to-foreign
			    ptr (v:x qv) (v:y qv)
			    (+ 0s0 x) (+ 0s0 y))))))
	      (across-c-ptr #'put arr))
	    (make-gpu-array arr)))
	 (indices (with-c-array
		      (arr (make-c-array nil :dimensions (* 6 size-x size-y)
					 :element-type :uint))
		    (let ((indices #(3 0 1 3 1 2)))
		      (labels ((put (ptr x)
				 (multiple-value-bind (quad-num n) (floor x 6)
				   (setf (cffi:mem-aref ptr :uint)
					 (+ (aref indices n) (* quad-num 4))))))
			(across-c-ptr #'put arr)))
		    (make-gpu-array arr))))
    (make-buffer-stream verts :index-array indices :retain-arrays t)))

;;----------------------------------------------------------------
