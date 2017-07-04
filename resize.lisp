(in-package :lark)


(defun down-to-nearest (x n)
  (* (floor x n) n))


(defun reshape (new-resolution)
  (let ((new-dimensions (list (down-to-nearest (v:x new-resolution) 8)
                              (down-to-nearest (v:y new-resolution) 8))))
    (setf (cepl.camera:camera-dimensions *camera*) new-dimensions)
    (free (render-state-gbuffer (game-state-render-state *game-state*)))
    (setf (render-state-gbuffer (game-state-render-state *game-state*))
          (make-gbuffer new-dimensions))))


(defun window-size-callback (size)
  (reshape size))
