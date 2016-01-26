(in-package lark)

(hasty:def-component transform ()
    ((position (cepl:v! 0 0 -20 0) :type (simple-array single-float (4)))
     (rotation (q:identity-quat) :type (simple-array single-float (4))))
  (when (has-renderable entity)
    nil
    ;; (update
    ;;  :rotation (v! (sin (/ (get-internal-real-time) 500)) 518.8038 0))
    ))
