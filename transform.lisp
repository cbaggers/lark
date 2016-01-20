(in-package #:lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 0) :type (simple-array single-float (3)))
     (rotation (q:identity-quat) :type (simple-array single-float (4)))
     (changed-this-frame nil :type boolean))
  nil)

(initialize-transform-system)
