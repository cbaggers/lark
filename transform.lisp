(in-package #:lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 -20 0) :type (simple-array single-float (4)))
     (rotation (v! 0 0 0 0) :type (simple-array single-float (4))))
  (when (has-renderable entity)
    (update
     :position (v! 0 0 -20 0)
     :rotation (v! (sin (/ (get-internal-real-time) 500)) 518.8038 0))))
