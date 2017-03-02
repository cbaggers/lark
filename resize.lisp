(in-package :lark)


(defun down-to-nearest (x n)
  (* (floor x n) n))


(defun reshape (new-resolution)
  (let ((new-dimensions (list (down-to-nearest (v:x new-resolution) 8)
                              (down-to-nearest (v:y new-resolution) 8))))
    (setf (cepl.camera:camera-dimensions *camera*) new-dimensions)
    (setf (gbuffer (render-state *game-state*))
          (make-gbuffer new-dimensions))))


(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (reshape (skitter:size-2d-vec event)))
