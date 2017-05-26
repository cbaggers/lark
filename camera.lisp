(in-package #:lark)

(defvar *world-up* (v! 0 1 0))

(defvar *camera* nil)


(defmethod pos ((c cepl.camera:camera))
  (camera-pos c))

(defmethod rot ((c cepl.camera:camera))
  (camera-rot c))

(defmethod (setf pos) (vec3 (c cepl.camera:camera))
  (assert (typep vec3 'rtg-math.types:vec3))
  (setf (camera-pos c) vec3))

(defmethod (setf rot) (vec3 (c cepl.camera:camera))
  (assert (typep vec4 'rtg-math.types:vec3))
  (setf (camera-rot c) vec3))
