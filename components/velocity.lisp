(in-package lark)

(hasty:def-component velocity (transform)
    ((velocity (cepl:v! 0 0 0)
	       :type 'rtg-math.types:vec3
	       :accessor velocity))
  (with-transform (position) entity
    (setf position (v3:+ position velocity))))
