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

(defun sphere-data (&key (radius 0.5) (lines-of-latitude 30)
                      (lines-of-longitude 30) (normals t) (tex-coords t))
  (declare (type (unsigned-byte 8) lines-of-longitude lines-of-latitude))
  ;; latitude  -  horizontal
  ;; longitude -  vertical
  (let ((faces (make-array (* 6 lines-of-latitude (* (1+ lines-of-longitude)))))
        (lat-angle (/ +pi+ lines-of-latitude))
        (lon-angle (/ (* 2.0 +pi+) lines-of-longitude))
        (f-index 0) (v-index 0))
    (list (loop :for lat :upto lines-of-latitude :append
             (let* ((part (* lat lat-angle))
                    (carry (* radius (sin part)))
                    (y (* radius (cos part))))
               (loop :for lon :upto (1- lines-of-longitude) :collect
                  (let* ((part (* lon lon-angle))
                         (x (* carry (sin part)))
                         (z (* carry (cos part)))
                         (pos (v! x y z)))
                    (when (not (eql lat lines-of-latitude))
                      (let ((part (+ v-index lines-of-longitude)))
                        (setf (aref faces f-index) (1+ part)
                              (aref faces (+ f-index 1))  v-index
                              (aref faces (+ f-index 2)) part
                              (aref faces (+ f-index 3)) (1+ part)
                              (aref faces (+ f-index 4)) (1+ v-index)
                              (aref faces (+ f-index 5)) v-index
                              f-index (+ 6 f-index)
                              v-index (1+ v-index))))
                    (if (not (or normals tex-coords))
                        pos
                        `(,pos
                          ,@(when normals `(,(v3:normalize pos)))
                          ,(v! 0 0 0)
                          ,@(when tex-coords
                                  `(,(v! (/ lon lines-of-longitude)
                                         (/ lat lines-of-latitude))))))))))
          (coerce faces 'list))))

(let (cached-gpu-data)
  (defun make-sphere-model (&key (radius 10.0) physics)
    (declare (ignore physics))
    (unless cached-gpu-data
      (setf cached-gpu-data
            (%prim->gpu-data
             (sphere-data :radius radius))))
    (yaksha:make-model-from-mesh
     (yaksha:make-mesh :stream cached-gpu-data
                       :textures nil))))

(defun %prim->gpu-data (data-from-dendrite)
  (cepl:make-buffer-stream
   (cepl:make-gpu-array (first data-from-dendrite)
                        :element-type 'yaksha:vertex)
   :index-array (cepl:make-gpu-array (second data-from-dendrite)
                                     :element-type :ushort)
   :retain-arrays t))
