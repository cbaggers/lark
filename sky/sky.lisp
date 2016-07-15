(in-package :assurance)

(defvar *sky-enabled* t)
(defvar *skybox-stream* nil)
(defvar *sky-cube-sampler* nil)

(defun enable-sky ()
  (setf *sky-enabled* t))

(defun disable-sky ()
  (setf *sky-enabled* nil))


(defun-g vert ((vert :vec3) &uniform (to-cam-space :mat4))
  (let ((pos (* to-cam-space (v! vert 1.0))))
    (values (s~ pos :xyww)
	    vert)))

(defun-g frag ((tc :vec3) &uniform (tex :sampler-cube))
  (/ (texture tex tc) 100))

(def-g-> skybox () #'vert #'frag)

(defun render-sky (camera)
  (when *sky-enabled*
    (gl:depth-func :lequal)
    (using-camera camera
      (with-fbo-bound ((post-buff-fbo (get-post-buff)))
	(let* ((transform (cepl.space:get-transform
			   *world-space*
			   (cepl.camera.base::base-camera-space camera)))
	       (no-translate (m4:from-mat3 (m4:to-mat3 transform)))
	       (to-clip (cepl.space:get-transform
			 (cepl.camera.base::base-camera-space camera)
			 *clip-space*)))
	  (map-g #'skybox *skybox-stream*
		 :tex qoob;; *sky-cube-sampler*
		 :to-cam-space (m4:* to-clip no-translate)))))
    (gl:depth-func :less)))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar (lambda (p)
                               (cepl.sdl2-image:load-image-to-c-array
                                (path p)))
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
  (setf *sky-cube-sampler*
	(sample
	 (make-cubemap-tex
	  "./sky/default-tex/left.png"
	  "./sky/default-tex/right.png"
	  "./sky/default-tex/up.png"
	  "./sky/default-tex/down.png"
	  "./sky/default-tex/front.png"
	  "./sky/default-tex/back.png"))))

(push #'init-sky-data *on-engine-init*)
