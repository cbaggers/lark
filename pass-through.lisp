(in-package :lark)

(defun-g pass-through-vert ((vert g-pt))
  (values (:smooth (v! (pos vert) 1))
	  (tex vert)))
