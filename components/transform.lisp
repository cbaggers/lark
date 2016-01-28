(in-package lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 -20 0) :type (simple-array single-float (4)))
     (rotation (q:identity-quat) :type (simple-array single-float (4))))
  nil)
