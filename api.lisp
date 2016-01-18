(in-package :lark)


(defun load-model (filepath &key physics)
  (declare (ignore physics))
  (yaksha:load-model filepath))

(let (cached-gpu-data)
  (defun make-cube (&key (size 10.0) physics)
    (declare (ignore physics))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (add-to-release-pool
	     (%prim->gpu-data
	      (dendrite.primitives:box-data
	       :width size :height size :depth size)))))
    (add-transform
     (add-mesh-renderable
      (hasty:entity!)
      :mesh (yaksha:make-mesh :stream cached-gpu-data
			      :textures nil)))))

(let (cached-gpu-data)
  (defun make-sphere (&key (radius 10.0) physics)
    (declare (ignore physics))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (add-to-release-pool
	     (%prim->gpu-data
	      (dendrite.primitives:sphere-data :radius radius)))))
    (add-transform
     (add-mesh-renderable
      (hasty:entity!)
      :mesh (yaksha:make-mesh :stream cached-gpu-data
			      :textures nil)))))




(defun %prim->gpu-data (data-from-dendrite)
  (jungl:make-buffer-stream
   (jungl:make-gpu-array (first data-from-dendrite)
			 :element-type 'yaksha:vertex)
   :index-array (jungl:make-gpu-array (second data-from-dendrite)
				      :element-type :ushort)
   :retain-arrays t))
