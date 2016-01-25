(in-package #:lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 -20 0) :type (simple-array single-float (4)))
     (rotation (q:identity-quat) :type (simple-array single-float (4))))
  (when (has-renderable entity)
    (let ((sin-time (sin (/ (get-internal-real-time) 1000.0))))
      (update
       :position (v! 1 0 -10 1)
       :rotation (q:identity-quat)))))
