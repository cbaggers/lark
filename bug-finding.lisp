(in-package #:lark)



(defun-g tst-v ((vert :vec3) &uniform (model-space vec-space))
  (labels ((foo ((x :vec4))
	     (in *world-space*
	       x)))
    (in model-space
      (foo (sv! (v! 0 0 0 0))))))


(defun-g tst-f ((col :vec4))
  (v! 0 0 0 0))


(def-g-> bah ()
  #'tst-v #'tst-f)
