(in-package :lark)

(defvar *sky-enabled* nil)
(defvar *skybox-stream* nil)
(defvar *sky-cube-texture* nil)

(defun enable-sky ()
  (setf *sky-enabled* t))

(defun disable-sky ()
  (setf *sky-enabled* nil))


(defun-g vert ((vert :vec3) &uniform (to-cam-space :mat4))
  (let ((pos (* to-cam-space (v! vert 1.0))))
    (values (s~ pos :xyww)
	    vert)))

(defun-g frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(defpipeline skybox () (g-> #'vert #'frag))

(defun render-sky ()
  (when *sky-enabled*
    (gl:depth-func :lequal)

    (break "
Heya future chris, this is past chris with a bug.
Just below this break is the calculation of to-clip. it should be inside that
'using-camera' but it breaks hard in there. I think it's because the starting
node is the route-restriction. Should be easy enough to fix")

    (let ((to-clip (jungl.space:get-transform
		    (cepl.camera.base::base-camera-space (eye-ccam *current-camera*))
		    *clip-space*)))
      (with-viewport (current-viewport)
	(using-camera *current-camera*
	  (let* ((transform (jungl.space:get-transform
			     *world-space*
			     (cepl.camera.base::base-camera-space ccam)))
		 (no-translate (m4:from-mat3 (m4:to-mat3 transform))))
	    (map-g #'skybox *skybox-stream*
		   :tex *sky-cube-texture*
		   :to-cam-space (m4:* to-clip no-translate))))))
    (gl:depth-func :less)))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar (lambda (p)
                               (cepl.devil:load-image-to-c-array
                                (path-relative-to-lark p)))
                             paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

(defun init-sky-data ()
  (let* ((bx (dendrite.primitives:cube-data
	      :size 2s0 :normals nil :tex-coords nil))
         (data (make-gpu-array (first bx) :element-type :vec3))
         (ind (make-gpu-array
	       (dendrite.primitives:swap-winding-order (second bx))
	       :element-type :ushort)))
    (setf *skybox-stream*
	  (make-buffer-stream data :index-array ind :retain-arrays t)))
  (setf *sky-cube-texture*
	(make-cubemap-tex
	 "./sky/default-tex/left.png"
	 "./sky/default-tex/right.png"
	 "./sky/default-tex/up.png"
	 "./sky/default-tex/down.png"
	 "./sky/default-tex/front.png"
	 "./sky/default-tex/back.png")))

(push #'init-sky-data *on-engine-init*)
