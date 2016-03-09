(in-package :lark)

(defvar *fullscreen-quad*)
(defvar *particle-stream*)
(defparameter *particle-resolution* '(1024 1024))
(defparameter *starting-positions* nil)
(defparameter *starting-velocities*
  (make-array *particle-resolution* :initial-element (v! 0 0 0)))

(deftclass particle-gbuffer
  (positions (make-texture *starting-positions*
			   :dimensions *particle-resolution*
			   :element-type :rgb32f))
  (positions-fbo nil)
  (velocities (make-texture *starting-velocities*
			    :dimensions *particle-resolution*
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
    (setf *particle-stream* (apply #'thing *particle-resolution*))
    (let ((arr (make-array *particle-resolution*
			   :element-type 'rtg-math.types:vec3)))
      (loop :for y :below (second *particle-resolution*) :do
	 (loop :for x :below (first *particle-resolution*) :do
	    (setf (aref arr x y) (v! (- (random 20.0) 10)
				     (- (random 20.0) 10)
				     0))))
      (setf *starting-positions* arr))
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

(defpipeline move-particles ()
    (g-> #'particle-vert #'update-particle-positions))

;;----------------------------------------------------------------

(defun-g update-particle-velocities ((tex-coord :vec2)
				     &uniform (positions :sampler-2d)
				     (velocities :sampler-2d))
  (let ((position (texture positions tex-coord))
	(velocity (texture velocities tex-coord)))
    (* (normalize (- (v! 0 0 -20 0) position)) 0.001)))

(defpipeline update-velocities ()
    (g-> #'particle-vert #'update-particle-velocities))

;;----------------------------------------------------------------

(defun-g place-particle ((vert :vec4) &uniform (positions :sampler-2d))
  (let* ((pos-index (v!int (int (floor (v:z vert))) (int (floor (v:w vert)))))
	 (particle-position (texel-fetch positions pos-index 0))
	 (corner-pos (v! (v:x vert) (v:y vert))))
    (values (in *clip-space*
	      (in *world-space*
		(p! (+ (v! corner-pos 0 0)
		       (v! 0 0 -500 0)
		       (* 100 particle-position)))))
	    (* (+ corner-pos (v! 1 1)) 0.5))))

(defun-g place-particle-frag ((tex-coord :vec2))
  (v! 1 0 1 0))

(defpipeline draw-particles ()
    (g-> #'place-particle #'place-particle-frag))

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

(defun thing (size-x size-y)
  (let* ((quad-verts (list (v! -1.0 -1.0) (v! 1.0 -1.0)
			   (v! 1.0 1.0) (v! -1.0 1.0)))
	 (data (make-array (* 4 size-x size-y)
			   :element-type 'rtg-math.types:vec4
			   :initial-element (v! 0 0 0 0)))
	 (verts (loop :for y :below size-y :do
		   (loop :for x :below size-x :do
		      (loop :for qv :in quad-verts :for i :from 0 :do
			 (let ((index (+ i (* x 4) (* y 4 size-x))))
			   (setf (aref data index)
				 (v! (v:x qv) (v:y qv) x y)))))
		   :finally (return (make-gpu-array data :element-type :vec4))))
	 (indices (with-c-array
		      (x (make-c-array nil :dimensions (* 6 size-x size-y)
				       :element-type :uint))
		    (let ((index #(3 0 1 3 1 2))
			  (offset 0))
		      (loop :for i :below (* size-x size-y 6) :by 6 :do
			 (loop :for e :across index :for j :from 0 :do
			    (setf (cffi:mem-aref (c-array-pointer x) :uint
						(+ i j))
				  (+ e offset)))
			 (incf offset 4)))
		    (make-gpu-array x))))
    (make-buffer-stream verts :index-array indices :retain-arrays t)))

;;----------------------------------------------------------------
