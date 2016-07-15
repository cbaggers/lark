(in-package #:lark)

(let (cached-gpu-data)
  (defun make-cube (&key (size 10.0))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (%prim->gpu-data
	     (dendrite.primitives:box-data
	      :width size :height size :depth size))))
    (make-thing :model (yaksha:make-model-from-mesh
			(yaksha:make-mesh :stream cached-gpu-data
					  :textures nil)))))

(let (cached-gpu-data)
  (defun make-box (&key (width 80.0) (height 1.0) (depth 80.0))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (%prim->gpu-data
	     (dendrite.primitives:box-data
	      :width width :height height :depth depth))))
    (make-thing :model (yaksha:make-model-from-mesh
			(yaksha:make-mesh :stream cached-gpu-data
					  :textures nil)))))

(let (cached-gpu-data)
  (defun make-sphere (&key (radius 10.0) physics)
    (declare (ignore physics))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (%prim->gpu-data
	     (dendrite.primitives:sphere-data :radius radius))))
    (make-thing :model (yaksha:make-model-from-mesh
			(yaksha:make-mesh :stream cached-gpu-data
					  :textures nil)))))

(defun %prim->gpu-data (data-from-dendrite)
  (cepl:make-buffer-stream
   (cepl:make-gpu-array (first data-from-dendrite)
			 :element-type 'yaksha:vertex)
   :index-array (cepl:make-gpu-array (second data-from-dendrite)
				      :element-type :ushort)
   :retain-arrays t))
