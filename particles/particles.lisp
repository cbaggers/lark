(in-package :lark)

(defvar *fullscreen-quad*)
(defvar *particle-stream*)
(defparameter *particle-resolution* '(1024 1024))
(defparameter *starting-positions* nil)
(defparameter *starting-velocities*
  (make-c-array (make-array *particle-resolution* :initial-element (v! 0 0 0))
		:element-type :half-vec3))
(defvar *cols* nil)

(defstruct-g wells
  (pos (:vec3 10)))

(deftclass particle-gbuffer
  (positions (sample (make-texture *starting-positions*
				   :element-type :rgb32f)))
  (positions-fbo nil)
  (velocities (sample (make-texture *starting-velocities*
				    :element-type :rgb16f)))
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
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-positions front))))
	    (particle-gbuffer-velocities-fbo front)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-velocities front)))))
      (setf (particle-gbuffer-positions-fbo back)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-positions back))))
	    (particle-gbuffer-velocities-fbo back)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-velocities back)))))
      result)))

(defun reset-particle-system (sys)
  (let ((front (particle-system-front-gbuffer sys))
	(back (particle-system-back-gbuffer sys)))
    (push-g *starting-positions*
	    (sampler-texture (particle-gbuffer-positions front)))
    (push-g *starting-positions*
	    (sampler-texture (particle-gbuffer-positions back)))

    (push-g *starting-velocities*
	    (sampler-texture (particle-gbuffer-velocities front)))
    (push-g *starting-velocities*
	    (sampler-texture (particle-gbuffer-velocities back)))
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
		 ;;(declare (ignore x y))
		 (setf (cffi:mem-aref ptr :float 0) (+ -10s0 (/ x (/ 1024s0 20s0)))
		       (cffi:mem-aref ptr :float 1) (+ -10s0 (/ y (/ 1024s0 20s0)))
		       (cffi:mem-aref ptr :float 2) 0s0)))
	(setf *starting-positions* (across-c-ptr #'init arr))))
    (setf *cols*
	  (sample
	   (cepl.devil:load-image-to-texture
	    "/home/baggers/Pictures/lisplogo.png")))
    t))

;;----------------------------------------------------------------

(defun-g particle-vert ((vert cepl:g-pt))
  (values (v! (pos vert) 1) (* (v! 1 -1) (cepl:tex vert))))

(defun-g update-particle-positions ((tex-coord :vec2)
				    &uniform (positions :sampler-2d)
				    (velocities :sampler-2d))
  (let ((position (texture positions tex-coord))
	(velocity (texture velocities tex-coord)))
    (+ (v! (s~ position :xyz) 0) velocity)))

(def-g-> move-particles ()
  #'particle-vert #'update-particle-positions)

;;----------------------------------------------------------------

(defun-g update-particle-velocities ((tex-coord :vec2)
				     &uniform (positions :sampler-2d)
				     (velocities :sampler-2d)
				     (well :vec3))
  (let* ((position (texture positions tex-coord))
	 (velocity (texture velocities tex-coord))
	 (dif (- (v! well 0) position)))
    (+ velocity
       (* (normalize dif) (sqrt (length dif)) 0.001))))

(def-g-> update-velocities ()
  #'particle-vert #'update-particle-velocities)

;;----------------------------------------------------------------

(defun-g place-particle ((vert :vec4) &uniform (positions :sampler-2d)
			 (logo :sampler-2d))
  (let* ((pos-index (v!int (int (floor (v:z vert))) (int (floor (v:w vert)))))
	 (particle-position (texel-fetch positions pos-index 0))
	 (corner-pos (v! (v:x vert) (v:y vert))))
    (values (in *clip-space*
	      (+ (v! (* corner-pos 4) 0 0)
		 (in *world-space*
		   (sv! (+ (v! 0 0 -500 0)
			   (* 100 particle-position))))))
    	    (* (+ corner-pos (v! 1 1)) 0.5)
	    (texel-fetch logo pos-index 0))))

(defun-g place-particle-frag ((tex-coord :vec2) (col :vec4))
  col)

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

    (with-fbo-bound ((particle-gbuffer-velocities-fbo destination)
    		     :with-blending nil)
      (map-g #'update-velocities *fullscreen-quad*
    	     :positions (particle-gbuffer-positions source)
    	     :velocities (particle-gbuffer-velocities source)
	     :well (v! 0 0 0)))

    (with-fbo-bound ((particle-gbuffer-positions-fbo destination)
    		     :with-blending nil)
      (map-g #'move-particles *fullscreen-quad*
    	     :positions (particle-gbuffer-positions source)
    	     :velocities (particle-gbuffer-velocities source)))
    (with-viewport (current-viewport)
	(using-camera *current-camera*
	  (map-g #'draw-particles *particle-stream*
		 :positions (particle-gbuffer-positions destination)
		 :logo *cols*)))))


;;----------------------------------------------------------------

;; (v! vert.x vert.y pos.u pos.v)

(defun make-particle-stream (size-x size-y)
  (let* ((quad-verts (vector (v! -1.0 -1.0) (v! 1.0 -1.0)
			     (v! 1.0 1.0) (v! -1.0 1.0)))
	 (verts
	  (with-c-array
	      (arr (make-c-array nil :dimensions (* 4 size-x size-y)
				 :element-type :vec4))
	    (labels ((put (ptr index)
		       (multiple-value-bind (y x) (floor (floor index 4) size-x)
			 (let ((qv (svref quad-verts (mod index 4))))
			   (setf (cffi:mem-aref ptr :float 0) (v:x qv)
				 (cffi:mem-aref ptr :float 1) (v:y qv)
				 (cffi:mem-aref ptr :float 2) (+ 0s0 x)
				 (cffi:mem-aref ptr :float 3) (+ 0s0 y))))))
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

(defun bloop (size-x size-y)
  (let ((quad-verts (vector (v! -1.0 -1.0) (v! 1.0 -1.0)
			    (v! 1.0 1.0) (v! -1.0 1.0))))
    (loop :for index :below (* 4 size-x size-y) :collect
       (multiple-value-bind (y x) (floor (floor index 4) size-x)
	 (let ((qv (svref quad-verts (mod index 4))))
	   (list (v:x qv) (v:y qv) (+ 0s0 x) (+ 0s0 y)))))))
