(in-package #:lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 -20) :type (simple-array single-float (3)))
     (rotation (q:identity-quat) :type (simple-array single-float (4)))
     (model->world (m4:identity) :type (simple-array single-float (16))))
  (update
   :model->world (m4:m* (m4:translation position)
			(q:to-matrix4 (q:normalize rotation)))))
