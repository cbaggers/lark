(in-package :lark)

(defvar *sky-enabled* nil)
(defvar *skybox-stream* nil)
(defvar *sky-cube-texture* nil)

(defun enable-sky ()
  (setf *sky-enabled* t))

(defun disable-sky ()
  (setf *sky-enabled* nil))


(defun-g vert ((vert :vec3) &uniform (to-cam-space :mat4) (pos :vec3))
  (let ((pos (* to-cam-space (v! (+ pos vert) 1.0))))
    (values (s~ pos :xyww)
	    vert)))

(defun-g frag ((tc :vec3) &uniform (tex :sampler-cube))
  (texture tex tc))

(defpipeline skybox () (g-> #'vert #'frag))

(defun render-sky ()
  (when *sky-enabled*
    (gl:depth-func :lequal)
    (with-viewport (current-viewport)
      (with-eye (ccam) *current-camera*
	(cepl.camera:using-camera ccam
	  (let* ((transform (jungl.space:get-transform
			     *world-space*
			     (cepl.camera.base::base-camera-space ccam)))
		 (no-translate (m4:from-mat3 (m4:to-mat3 transform))))
	    (map-g #'skybox *skybox-stream*
		   :tex *sky-cube-texture*
		   :to-cam-space no-translate)))))
    (gl:depth-func :less)))

(defun make-cubemap-tex (&rest paths)
  (with-c-arrays (ca (mapcar (lambda (p)
                               (cepl.devil:load-image-to-c-array
                                (path-relative-to-lark p)))
                             paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

(defun init-sky-data ()
  (let* ((bx (dendrite.primitives:box-data :normals nil :tex-coords nil))
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
