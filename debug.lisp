(in-package :lark)

;;----------------------------------------------------------------------

(defun-g debug-draw-sampler-frag ((tc :vec2) &uniform (s :sampler-2d))
  (texture s tc 0))

(def-g-> debug-draw-sampler ()
  (pass-through-vert g-pt)
  (debug-draw-sampler-frag :vec2))

(defun draw-sampler (sampler)
  (cls)
  (map-g #'debug-draw-sampler *quad-stream* :s sampler)
  (swap))

(defun-g green-frag ((tc :vec2) &uniform (s :sampler-2d))
  (v! 0 1 0 1))

(def-g-> green-pass ()
  (pass-through-vert g-pt)
  (green-frag :vec2))

(defun green ()
  (map-g #'green-pass *quad-stream*))
