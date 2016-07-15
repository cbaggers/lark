(in-package :assurance)


(defun-g pass-through-vert ((vert g-pt))
  (values (v! (pos vert) 1)
	  (tex vert)))
