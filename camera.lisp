(in-package #:lark)

(defvar *world-up* (v! 0 1 0))

(defvar *camera* nil)


(defmethod pos ((c cepl.camera:camera))
  (camera-pos c))

(defmethod rot ((c cepl.camera:camera))
  (camera-rot c))

(defmethod (setf pos) (vec4 (c cepl.camera:camera))
  (assert (typep vec4 'rtg-math.types:vec4))
  (setf (camera-pos c) vec4))

(defmethod (setf rot) (vec4 (c cepl.camera:camera))
  (assert (typep vec4 'rtg-math.types:vec4))
  (setf (camera-rot c) vec4))
