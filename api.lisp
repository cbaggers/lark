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
    (hasty:register-entity
     (add-transform
      (add-renderable
       (hasty:entity!)
       :model (yaksha:make-model-from-mesh
	       (yaksha:make-mesh :stream cached-gpu-data
				 :textures nil)))))))

(let (cached-gpu-data)
  (defun make-sphere (&key (radius 10.0) physics)
    (declare (ignore physics))
    (unless cached-gpu-data
      (setf cached-gpu-data
	    (add-to-release-pool
	     (%prim->gpu-data
	      (dendrite.primitives:sphere-data :radius radius)))))
    (hasty:register-entity
     (add-transform
      (add-renderable
       (hasty:entity!)
       :model (yaksha:make-model-from-mesh
	       (yaksha:make-mesh :stream cached-gpu-data
				 :textures nil)))))))

(defun make-widget ()
  (let* ((verts (make-gpu-array `((,(v! +1  +1  +1) ,(v! 0 0 0) ,(v! 0.1 0.8))
				  (,(v! -1  -1  +1) ,(v! 0 0 0) ,(v! 0.2 0.4))
				  (,(v! -1  +1  -1) ,(v! 0 0 0) ,(v! 0.5 0.9))
				  (,(v! +1  -1  -1) ,(v! 0 0 0) ,(v! 1 1))
				  (,(v! -1  -1  -1) ,(v! 0 0 0) ,(v! 0.5 0.5))
				  (,(v! +1  +1  -1) ,(v! 0 0 0) ,(v! 1 0))
				  (,(v! +1  -1  +1) ,(v! 0 0 0) ,(v! 0 1))
				  (,(v! -1  +1  +1) ,(v! 0 0 0) ,(v! 0.2 0.8)))
				:element-type 'yaksha:vertex :dimensions 8))
	 (indicies (make-gpu-array '(0 2 1   1 3 0   2 0 3   3 1 2
				     5 6 4   4 7 5   7 4 6   6 5 7)
				   :dimensions 24 :element-type :unsigned-short))
	 (e-stream (make-buffer-stream verts :index-array indicies
				       :retain-arrays t)))
    (hasty:register-entity
     (add-transform
      (add-renderable
       (hasty:entity!)
       :model
       (make-model-from-mesh
	(yaksha:make-mesh :stream e-stream
			  :textures nil)))))))




(defun %prim->gpu-data (data-from-dendrite)
  (jungl:make-buffer-stream
   (jungl:make-gpu-array (first data-from-dendrite)
			 :element-type 'yaksha:vertex)
   :index-array (jungl:make-gpu-array (second data-from-dendrite)
				      :element-type :ushort)
   :retain-arrays t))

(defmethod free ((object hasty:entity))
  ;; todo cleanup
  (hasty::unregister-entity object)
  t)
